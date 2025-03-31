#if defined (__GNUC__)                                            
#if ((__GNUC__ * 100) + __GNUC_MINOR__) >= 402                    
#pragma GCC diagnostic ignored "-Wconversion"                     
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"  
#pragma GCC diagnostic ignored "-Wmissing-prototypes"             
#pragma GCC diagnostic ignored "-Wnested-externs"                 
#pragma GCC diagnostic ignored "-Wold-style-definition"           
#pragma GCC diagnostic ignored "-Wredundant-decls"                
#pragma GCC diagnostic ignored "-Wsign-compare"                   
#pragma GCC diagnostic ignored "-Wsign-conversion"                
#pragma GCC diagnostic ignored "-Wstrict-overflow"                
#pragma GCC diagnostic ignored "-Wstrict-prototypes"              
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"           
#if !defined (__clang__)                                          
#pragma GCC diagnostic ignored "-Wlarger-than="                   
#pragma GCC diagnostic ignored "-Wsuggest-attribute=const"        
#pragma GCC diagnostic ignored "-Wsuggest-attribute=pure"         
#endif                                                            
#pragma GCC diagnostic ignored "-Wswitch-default"                 
#pragma GCC diagnostic ignored "-Wunused-function"                
#pragma GCC diagnostic ignored "-Wunused-macros"                  
#pragma GCC diagnostic ignored "-Wunused-parameter"               
#endif                                                            
#if ((__GNUC__ * 100) + __GNUC_MINOR__) >= 600                    
#pragma GCC diagnostic ignored "-Wnull-dereference"               
#endif                                                            
#elif defined _MSC_VER                                            
#pragma warning(push, 1)                                          
#endif                                                            
/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         H5LTyyparse
#define yylex           H5LTyylex
#define yyerror         H5LTyyerror
#define yydebug         H5LTyydebug
#define yynerrs         H5LTyynerrs
#define yylval          H5LTyylval
#define yychar          H5LTyychar

/* First part of user prologue.  */
#line 19 "hl/src//H5LTparse.y"

#include <stdio.h>
#include <string.h>
#include <hdf5.h>

#include "H5private.h"

extern int yylex(void);
extern int yyerror(const char *);

#define STACK_SIZE      16

/*structure for compound type information*/
struct cmpd_info {
    hid_t       id;             /*type ID*/
    bool        is_field;       /*flag to lexer for compound member*/
    bool        first_memb;     /*flag for first compound member*/
};

/*stack for nested compound type*/
static struct cmpd_info cmpd_stack[STACK_SIZE] = {
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1} };

static int csindex = -1;                /*pointer to the top of compound stack*/

/*structure for array type information*/
struct arr_info {
    hsize_t             dims[H5S_MAX_RANK];     /*size of each dimension, limited to 32 dimensions*/
    unsigned            ndims;                  /*number of dimensions*/
    bool                is_dim;                 /*flag to lexer for dimension*/
};
/*stack for nested array type*/
static struct arr_info arr_stack[STACK_SIZE];
static int asindex = -1;               /*pointer to the top of array stack*/ 

static H5T_str_t   str_pad;                /*variable for string padding*/
static H5T_cset_t  str_cset;               /*variable for string character set*/
static bool        is_variable = 0;        /*variable for variable-length string*/
static size_t      str_size;               /*variable for string size*/
   
static hid_t       enum_id;                /*type ID*/
static bool        is_enum = 0;            /*flag to lexer for enum type*/
static bool        is_enum_memb = 0;       /*flag to lexer for enum member*/
static char*       enum_memb_symbol;       /*enum member symbol string*/


#line 128 "hl/src//H5LTparse.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "H5LTparse.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_H5T_STD_I8BE_TOKEN = 3,         /* H5T_STD_I8BE_TOKEN  */
  YYSYMBOL_H5T_STD_I8LE_TOKEN = 4,         /* H5T_STD_I8LE_TOKEN  */
  YYSYMBOL_H5T_STD_I16BE_TOKEN = 5,        /* H5T_STD_I16BE_TOKEN  */
  YYSYMBOL_H5T_STD_I16LE_TOKEN = 6,        /* H5T_STD_I16LE_TOKEN  */
  YYSYMBOL_H5T_STD_I32BE_TOKEN = 7,        /* H5T_STD_I32BE_TOKEN  */
  YYSYMBOL_H5T_STD_I32LE_TOKEN = 8,        /* H5T_STD_I32LE_TOKEN  */
  YYSYMBOL_H5T_STD_I64BE_TOKEN = 9,        /* H5T_STD_I64BE_TOKEN  */
  YYSYMBOL_H5T_STD_I64LE_TOKEN = 10,       /* H5T_STD_I64LE_TOKEN  */
  YYSYMBOL_H5T_STD_U8BE_TOKEN = 11,        /* H5T_STD_U8BE_TOKEN  */
  YYSYMBOL_H5T_STD_U8LE_TOKEN = 12,        /* H5T_STD_U8LE_TOKEN  */
  YYSYMBOL_H5T_STD_U16BE_TOKEN = 13,       /* H5T_STD_U16BE_TOKEN  */
  YYSYMBOL_H5T_STD_U16LE_TOKEN = 14,       /* H5T_STD_U16LE_TOKEN  */
  YYSYMBOL_H5T_STD_U32BE_TOKEN = 15,       /* H5T_STD_U32BE_TOKEN  */
  YYSYMBOL_H5T_STD_U32LE_TOKEN = 16,       /* H5T_STD_U32LE_TOKEN  */
  YYSYMBOL_H5T_STD_U64BE_TOKEN = 17,       /* H5T_STD_U64BE_TOKEN  */
  YYSYMBOL_H5T_STD_U64LE_TOKEN = 18,       /* H5T_STD_U64LE_TOKEN  */
  YYSYMBOL_H5T_NATIVE_CHAR_TOKEN = 19,     /* H5T_NATIVE_CHAR_TOKEN  */
  YYSYMBOL_H5T_NATIVE_SCHAR_TOKEN = 20,    /* H5T_NATIVE_SCHAR_TOKEN  */
  YYSYMBOL_H5T_NATIVE_UCHAR_TOKEN = 21,    /* H5T_NATIVE_UCHAR_TOKEN  */
  YYSYMBOL_H5T_NATIVE_SHORT_TOKEN = 22,    /* H5T_NATIVE_SHORT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_USHORT_TOKEN = 23,   /* H5T_NATIVE_USHORT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_INT_TOKEN = 24,      /* H5T_NATIVE_INT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_UINT_TOKEN = 25,     /* H5T_NATIVE_UINT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_LONG_TOKEN = 26,     /* H5T_NATIVE_LONG_TOKEN  */
  YYSYMBOL_H5T_NATIVE_ULONG_TOKEN = 27,    /* H5T_NATIVE_ULONG_TOKEN  */
  YYSYMBOL_H5T_NATIVE_LLONG_TOKEN = 28,    /* H5T_NATIVE_LLONG_TOKEN  */
  YYSYMBOL_H5T_NATIVE_ULLONG_TOKEN = 29,   /* H5T_NATIVE_ULLONG_TOKEN  */
  YYSYMBOL_H5T_IEEE_F16BE_TOKEN = 30,      /* H5T_IEEE_F16BE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F16LE_TOKEN = 31,      /* H5T_IEEE_F16LE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F32BE_TOKEN = 32,      /* H5T_IEEE_F32BE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F32LE_TOKEN = 33,      /* H5T_IEEE_F32LE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F64BE_TOKEN = 34,      /* H5T_IEEE_F64BE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F64LE_TOKEN = 35,      /* H5T_IEEE_F64LE_TOKEN  */
  YYSYMBOL_H5T_FLOAT_BFLOAT16BE_TOKEN = 36, /* H5T_FLOAT_BFLOAT16BE_TOKEN  */
  YYSYMBOL_H5T_FLOAT_BFLOAT16LE_TOKEN = 37, /* H5T_FLOAT_BFLOAT16LE_TOKEN  */
  YYSYMBOL_H5T_FLOAT_F8E4M3_TOKEN = 38,    /* H5T_FLOAT_F8E4M3_TOKEN  */
  YYSYMBOL_H5T_FLOAT_F8E5M2_TOKEN = 39,    /* H5T_FLOAT_F8E5M2_TOKEN  */
  YYSYMBOL_H5T_FLOAT_F6E2M3_TOKEN = 40,    /* H5T_FLOAT_F6E2M3_TOKEN  */
  YYSYMBOL_H5T_FLOAT_F6E3M2_TOKEN = 41,    /* H5T_FLOAT_F6E3M2_TOKEN  */
  YYSYMBOL_H5T_NATIVE_FLOAT16_TOKEN = 42,  /* H5T_NATIVE_FLOAT16_TOKEN  */
  YYSYMBOL_H5T_NATIVE_FLOAT_TOKEN = 43,    /* H5T_NATIVE_FLOAT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_DOUBLE_TOKEN = 44,   /* H5T_NATIVE_DOUBLE_TOKEN  */
  YYSYMBOL_H5T_NATIVE_LDOUBLE_TOKEN = 45,  /* H5T_NATIVE_LDOUBLE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F16BE_TOKEN = 46, /* H5T_COMPLEX_IEEE_F16BE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F16LE_TOKEN = 47, /* H5T_COMPLEX_IEEE_F16LE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F32BE_TOKEN = 48, /* H5T_COMPLEX_IEEE_F32BE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F32LE_TOKEN = 49, /* H5T_COMPLEX_IEEE_F32LE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F64BE_TOKEN = 50, /* H5T_COMPLEX_IEEE_F64BE_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_IEEE_F64LE_TOKEN = 51, /* H5T_COMPLEX_IEEE_F64LE_TOKEN  */
  YYSYMBOL_H5T_NATIVE_FLOAT_COMPLEX_TOKEN = 52, /* H5T_NATIVE_FLOAT_COMPLEX_TOKEN  */
  YYSYMBOL_H5T_NATIVE_DOUBLE_COMPLEX_TOKEN = 53, /* H5T_NATIVE_DOUBLE_COMPLEX_TOKEN  */
  YYSYMBOL_H5T_NATIVE_LDOUBLE_COMPLEX_TOKEN = 54, /* H5T_NATIVE_LDOUBLE_COMPLEX_TOKEN  */
  YYSYMBOL_H5T_STRING_TOKEN = 55,          /* H5T_STRING_TOKEN  */
  YYSYMBOL_STRSIZE_TOKEN = 56,             /* STRSIZE_TOKEN  */
  YYSYMBOL_STRPAD_TOKEN = 57,              /* STRPAD_TOKEN  */
  YYSYMBOL_CSET_TOKEN = 58,                /* CSET_TOKEN  */
  YYSYMBOL_CTYPE_TOKEN = 59,               /* CTYPE_TOKEN  */
  YYSYMBOL_H5T_VARIABLE_TOKEN = 60,        /* H5T_VARIABLE_TOKEN  */
  YYSYMBOL_H5T_STR_NULLTERM_TOKEN = 61,    /* H5T_STR_NULLTERM_TOKEN  */
  YYSYMBOL_H5T_STR_NULLPAD_TOKEN = 62,     /* H5T_STR_NULLPAD_TOKEN  */
  YYSYMBOL_H5T_STR_SPACEPAD_TOKEN = 63,    /* H5T_STR_SPACEPAD_TOKEN  */
  YYSYMBOL_H5T_CSET_ASCII_TOKEN = 64,      /* H5T_CSET_ASCII_TOKEN  */
  YYSYMBOL_H5T_CSET_UTF8_TOKEN = 65,       /* H5T_CSET_UTF8_TOKEN  */
  YYSYMBOL_H5T_C_S1_TOKEN = 66,            /* H5T_C_S1_TOKEN  */
  YYSYMBOL_H5T_FORTRAN_S1_TOKEN = 67,      /* H5T_FORTRAN_S1_TOKEN  */
  YYSYMBOL_H5T_OPAQUE_TOKEN = 68,          /* H5T_OPAQUE_TOKEN  */
  YYSYMBOL_OPQ_SIZE_TOKEN = 69,            /* OPQ_SIZE_TOKEN  */
  YYSYMBOL_OPQ_TAG_TOKEN = 70,             /* OPQ_TAG_TOKEN  */
  YYSYMBOL_H5T_COMPOUND_TOKEN = 71,        /* H5T_COMPOUND_TOKEN  */
  YYSYMBOL_H5T_ENUM_TOKEN = 72,            /* H5T_ENUM_TOKEN  */
  YYSYMBOL_H5T_ARRAY_TOKEN = 73,           /* H5T_ARRAY_TOKEN  */
  YYSYMBOL_H5T_VLEN_TOKEN = 74,            /* H5T_VLEN_TOKEN  */
  YYSYMBOL_H5T_COMPLEX_TOKEN = 75,         /* H5T_COMPLEX_TOKEN  */
  YYSYMBOL_STRING = 76,                    /* STRING  */
  YYSYMBOL_NUMBER = 77,                    /* NUMBER  */
  YYSYMBOL_78_ = 78,                       /* '{'  */
  YYSYMBOL_79_ = 79,                       /* '}'  */
  YYSYMBOL_80_ = 80,                       /* '['  */
  YYSYMBOL_81_ = 81,                       /* ']'  */
  YYSYMBOL_82_ = 82,                       /* ':'  */
  YYSYMBOL_83_ = 83,                       /* ';'  */
  YYSYMBOL_YYACCEPT = 84,                  /* $accept  */
  YYSYMBOL_start = 85,                     /* start  */
  YYSYMBOL_ddl_type = 86,                  /* ddl_type  */
  YYSYMBOL_atomic_type = 87,               /* atomic_type  */
  YYSYMBOL_integer_type = 88,              /* integer_type  */
  YYSYMBOL_fp_type = 89,                   /* fp_type  */
  YYSYMBOL_compound_type = 90,             /* compound_type  */
  YYSYMBOL_91_1 = 91,                      /* $@1  */
  YYSYMBOL_memb_list = 92,                 /* memb_list  */
  YYSYMBOL_memb_def = 93,                  /* memb_def  */
  YYSYMBOL_94_2 = 94,                      /* $@2  */
  YYSYMBOL_field_name = 95,                /* field_name  */
  YYSYMBOL_field_offset = 96,              /* field_offset  */
  YYSYMBOL_offset = 97,                    /* offset  */
  YYSYMBOL_array_type = 98,                /* array_type  */
  YYSYMBOL_99_3 = 99,                      /* $@3  */
  YYSYMBOL_dim_list = 100,                 /* dim_list  */
  YYSYMBOL_dim = 101,                      /* dim  */
  YYSYMBOL_102_4 = 102,                    /* $@4  */
  YYSYMBOL_103_5 = 103,                    /* $@5  */
  YYSYMBOL_dimsize = 104,                  /* dimsize  */
  YYSYMBOL_vlen_type = 105,                /* vlen_type  */
  YYSYMBOL_complex_type = 106,             /* complex_type  */
  YYSYMBOL_opaque_type = 107,              /* opaque_type  */
  YYSYMBOL_108_6 = 108,                    /* @6  */
  YYSYMBOL_109_7 = 109,                    /* $@7  */
  YYSYMBOL_opaque_size = 110,              /* opaque_size  */
  YYSYMBOL_opaque_tag = 111,               /* opaque_tag  */
  YYSYMBOL_string_type = 112,              /* string_type  */
  YYSYMBOL_113_8 = 113,                    /* $@8  */
  YYSYMBOL_114_9 = 114,                    /* $@9  */
  YYSYMBOL_115_10 = 115,                   /* $@10  */
  YYSYMBOL_116_11 = 116,                   /* @11  */
  YYSYMBOL_strsize = 117,                  /* strsize  */
  YYSYMBOL_strpad = 118,                   /* strpad  */
  YYSYMBOL_cset = 119,                     /* cset  */
  YYSYMBOL_ctype = 120,                    /* ctype  */
  YYSYMBOL_enum_type = 121,                /* enum_type  */
  YYSYMBOL_122_12 = 122,                   /* $@12  */
  YYSYMBOL_enum_list = 123,                /* enum_list  */
  YYSYMBOL_enum_def = 124,                 /* enum_def  */
  YYSYMBOL_125_13 = 125,                   /* $@13  */
  YYSYMBOL_enum_symbol = 126,              /* enum_symbol  */
  YYSYMBOL_enum_val = 127                  /* enum_val  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  79
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   257

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  84
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  44
/* YYNRULES -- Number of rules.  */
#define YYNRULES  112
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  157

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   332


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    82,    83,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    80,     2,    81,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    78,     2,    79,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   111,   111,   112,   114,   115,   116,   117,   118,   120,
     121,   122,   123,   124,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   175,   174,   183,
     184,   186,   186,   223,   231,   232,   235,   237,   237,   246,
     247,   249,   250,   249,   257,   260,   264,   265,   266,   267,
     268,   269,   270,   271,   272,   273,   280,   285,   277,   292,
     294,   299,   306,   315,   322,   296,   346,   347,   349,   350,
     351,   353,   354,   356,   357,   361,   360,   365,   366,   368,
     368,   418,   420
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "H5T_STD_I8BE_TOKEN",
  "H5T_STD_I8LE_TOKEN", "H5T_STD_I16BE_TOKEN", "H5T_STD_I16LE_TOKEN",
  "H5T_STD_I32BE_TOKEN", "H5T_STD_I32LE_TOKEN", "H5T_STD_I64BE_TOKEN",
  "H5T_STD_I64LE_TOKEN", "H5T_STD_U8BE_TOKEN", "H5T_STD_U8LE_TOKEN",
  "H5T_STD_U16BE_TOKEN", "H5T_STD_U16LE_TOKEN", "H5T_STD_U32BE_TOKEN",
  "H5T_STD_U32LE_TOKEN", "H5T_STD_U64BE_TOKEN", "H5T_STD_U64LE_TOKEN",
  "H5T_NATIVE_CHAR_TOKEN", "H5T_NATIVE_SCHAR_TOKEN",
  "H5T_NATIVE_UCHAR_TOKEN", "H5T_NATIVE_SHORT_TOKEN",
  "H5T_NATIVE_USHORT_TOKEN", "H5T_NATIVE_INT_TOKEN",
  "H5T_NATIVE_UINT_TOKEN", "H5T_NATIVE_LONG_TOKEN",
  "H5T_NATIVE_ULONG_TOKEN", "H5T_NATIVE_LLONG_TOKEN",
  "H5T_NATIVE_ULLONG_TOKEN", "H5T_IEEE_F16BE_TOKEN",
  "H5T_IEEE_F16LE_TOKEN", "H5T_IEEE_F32BE_TOKEN", "H5T_IEEE_F32LE_TOKEN",
  "H5T_IEEE_F64BE_TOKEN", "H5T_IEEE_F64LE_TOKEN",
  "H5T_FLOAT_BFLOAT16BE_TOKEN", "H5T_FLOAT_BFLOAT16LE_TOKEN",
  "H5T_FLOAT_F8E4M3_TOKEN", "H5T_FLOAT_F8E5M2_TOKEN",
  "H5T_FLOAT_F6E2M3_TOKEN", "H5T_FLOAT_F6E3M2_TOKEN",
  "H5T_NATIVE_FLOAT16_TOKEN", "H5T_NATIVE_FLOAT_TOKEN",
  "H5T_NATIVE_DOUBLE_TOKEN", "H5T_NATIVE_LDOUBLE_TOKEN",
  "H5T_COMPLEX_IEEE_F16BE_TOKEN", "H5T_COMPLEX_IEEE_F16LE_TOKEN",
  "H5T_COMPLEX_IEEE_F32BE_TOKEN", "H5T_COMPLEX_IEEE_F32LE_TOKEN",
  "H5T_COMPLEX_IEEE_F64BE_TOKEN", "H5T_COMPLEX_IEEE_F64LE_TOKEN",
  "H5T_NATIVE_FLOAT_COMPLEX_TOKEN", "H5T_NATIVE_DOUBLE_COMPLEX_TOKEN",
  "H5T_NATIVE_LDOUBLE_COMPLEX_TOKEN", "H5T_STRING_TOKEN", "STRSIZE_TOKEN",
  "STRPAD_TOKEN", "CSET_TOKEN", "CTYPE_TOKEN", "H5T_VARIABLE_TOKEN",
  "H5T_STR_NULLTERM_TOKEN", "H5T_STR_NULLPAD_TOKEN",
  "H5T_STR_SPACEPAD_TOKEN", "H5T_CSET_ASCII_TOKEN", "H5T_CSET_UTF8_TOKEN",
  "H5T_C_S1_TOKEN", "H5T_FORTRAN_S1_TOKEN", "H5T_OPAQUE_TOKEN",
  "OPQ_SIZE_TOKEN", "OPQ_TAG_TOKEN", "H5T_COMPOUND_TOKEN",
  "H5T_ENUM_TOKEN", "H5T_ARRAY_TOKEN", "H5T_VLEN_TOKEN",
  "H5T_COMPLEX_TOKEN", "STRING", "NUMBER", "'{'", "'}'", "'['", "']'",
  "':'", "';'", "$accept", "start", "ddl_type", "atomic_type",
  "integer_type", "fp_type", "compound_type", "$@1", "memb_list",
  "memb_def", "$@2", "field_name", "field_offset", "offset", "array_type",
  "$@3", "dim_list", "dim", "$@4", "$@5", "dimsize", "vlen_type",
  "complex_type", "opaque_type", "@6", "$@7", "opaque_size", "opaque_tag",
  "string_type", "$@8", "$@9", "$@10", "@11", "strsize", "strpad", "cset",
  "ctype", "enum_type", "$@12", "enum_list", "enum_def", "$@13",
  "enum_symbol", "enum_val", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-24)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     152,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -21,   -15,   -24,   -14,   -24,    -4,    -2,
     131,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,    76,    64,    57,   225,    58,   152,   152,   -24,
      74,    60,   -24,    55,   -24,    61,    62,   -24,   -24,    56,
     -24,    59,    75,   -24,    -3,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,    65,   -24,    88,    82,    77,   -23,
     132,   -24,    -1,   134,   -24,   126,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   128,   -24,   129,   136,   133,
     137,   138,   -24,   -24,   -24,   -24,   -24,   -24,   135,   -24,
     157,   142,   -24,   -10,   -24,   -24,   -24,   139,   -24,   158,
       0,   -24,   -24,   172,   -24,   177,   -24
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    80,    79,    82,    81,    84,    83,
      76,    77,    78,     0,     0,    57,     0,    67,     0,     0,
       0,     3,     4,     9,    10,     5,     6,     7,     8,    13,
      11,    12,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,    59,     0,    69,     0,     0,    96,    97,     0,
      89,     0,     0,   105,     0,    75,    85,    91,    86,    58,
      61,    60,   107,    71,     0,    70,     0,     0,     0,     0,
       0,    68,     0,     0,    63,    64,   111,   106,   108,   109,
      74,    72,    98,    99,   100,     0,    90,     0,     0,     0,
       0,     0,    92,    87,    66,    65,    62,   112,     0,    73,
       0,     0,   110,     0,    88,   101,   102,     0,    93,     0,
       0,   103,   104,     0,    94,     0,    95
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -24,   -24,   -19,   -24,   182,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   -24
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    60,    61,    62,    63,    64,    65,    74,    92,   101,
     108,   115,   129,   135,    66,    76,    94,   105,   110,   131,
     121,    67,    68,    69,   107,   141,    91,   127,    70,   106,
     140,   149,   155,    89,   125,   147,   153,    71,   102,   109,
     118,   130,   119,   138
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       1,     2,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,   116,   145,   146,   117,    72,    85,    86,
     122,   123,   124,    73,    75,    54,   151,   152,    55,    56,
      57,    58,    59,   100,    77,   104,    78,   103,     1,     2,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    79,    80,    81,    87,    82,    84,    90,    93,    97,
      95,    96,    98,    54,   111,   112,    55,    56,    57,    58,
      59,    88,   113,   114,    99,     1,     2,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,   128,   120,
     126,   132,   133,   134,   137,   143,   136,   150,   142,   139,
      54,   144,   148,    55,    56,    57,    58,    59,     1,     2,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,   154,   156,    83
};

static const yytype_int8 yycheck[] =
{
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    76,    64,    65,    79,    78,    77,    78,
      61,    62,    63,    78,    78,    68,    66,    67,    71,    72,
      73,    74,    75,    92,    78,    94,    78,    80,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,    56,    69,    60,    78,    78,    77,    83,    83,
      79,    79,    83,    68,    79,    57,    71,    72,    73,    74,
      75,    77,    70,    76,    79,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    82,    77,
      76,    83,    83,    77,    77,    58,    83,    59,    83,    81,
      68,    79,    83,    71,    72,    73,    74,    75,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    83,    79,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    68,    71,    72,    73,    74,    75,
      85,    86,    87,    88,    89,    90,    98,   105,   106,   107,
     112,   121,    78,    78,    91,    78,    99,    78,    78,     0,
      56,    69,    78,    88,    78,    86,    86,    60,    77,   117,
      77,   110,    92,    83,   100,    79,    79,    83,    83,    79,
      86,    93,   122,    80,    86,   101,   113,   108,    94,   123,
     102,    79,    57,    70,    76,    95,    76,    79,   124,   126,
      77,   104,    61,    62,    63,   118,    76,   111,    82,    96,
     125,   103,    83,    83,    77,    97,    83,    77,   127,    81,
     114,   109,    83,    58,    79,    64,    65,   119,    83,   115,
      59,    66,    67,   120,    83,   116,    79
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    84,    85,    85,    86,    86,    86,    86,    86,    87,
      87,    87,    87,    87,    88,    88,    88,    88,    88,    88,
      88,    88,    88,    88,    88,    88,    88,    88,    88,    88,
      88,    88,    88,    88,    88,    88,    88,    88,    88,    88,
      88,    89,    89,    89,    89,    89,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    91,    90,    92,
      92,    94,    93,    95,    96,    96,    97,    99,    98,   100,
     100,   102,   103,   101,   104,   105,   106,   106,   106,   106,
     106,   106,   106,   106,   106,   106,   108,   109,   107,   110,
     111,   113,   114,   115,   116,   112,   117,   117,   118,   118,
     118,   119,   119,   120,   120,   122,   121,   123,   123,   125,
     124,   126,   127
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     5,     0,
       2,     0,     5,     1,     0,     2,     1,     0,     6,     0,
       2,     0,     0,     5,     1,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     0,     0,    11,     1,
       1,     0,     0,     0,     0,    19,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     7,     0,     2,     0,
       4,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

hid_t
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* start: %empty  */
#line 111 "hl/src//H5LTparse.y"
                { memset(arr_stack, 0, STACK_SIZE*sizeof(struct arr_info)); /*initialize here?*/ }
#line 1409 "hl/src//H5LTparse.c"
    break;

  case 3: /* start: ddl_type  */
#line 112 "hl/src//H5LTparse.y"
                          { return (yyval.hid);}
#line 1415 "hl/src//H5LTparse.c"
    break;

  case 14: /* integer_type: H5T_STD_I8BE_TOKEN  */
#line 127 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I8BE); }
#line 1421 "hl/src//H5LTparse.c"
    break;

  case 15: /* integer_type: H5T_STD_I8LE_TOKEN  */
#line 128 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I8LE); }
#line 1427 "hl/src//H5LTparse.c"
    break;

  case 16: /* integer_type: H5T_STD_I16BE_TOKEN  */
#line 129 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I16BE); }
#line 1433 "hl/src//H5LTparse.c"
    break;

  case 17: /* integer_type: H5T_STD_I16LE_TOKEN  */
#line 130 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I16LE); }
#line 1439 "hl/src//H5LTparse.c"
    break;

  case 18: /* integer_type: H5T_STD_I32BE_TOKEN  */
#line 131 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I32BE); }
#line 1445 "hl/src//H5LTparse.c"
    break;

  case 19: /* integer_type: H5T_STD_I32LE_TOKEN  */
#line 132 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I32LE); }
#line 1451 "hl/src//H5LTparse.c"
    break;

  case 20: /* integer_type: H5T_STD_I64BE_TOKEN  */
#line 133 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I64BE); }
#line 1457 "hl/src//H5LTparse.c"
    break;

  case 21: /* integer_type: H5T_STD_I64LE_TOKEN  */
#line 134 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I64LE); }
#line 1463 "hl/src//H5LTparse.c"
    break;

  case 22: /* integer_type: H5T_STD_U8BE_TOKEN  */
#line 135 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U8BE); }
#line 1469 "hl/src//H5LTparse.c"
    break;

  case 23: /* integer_type: H5T_STD_U8LE_TOKEN  */
#line 136 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U8LE); }
#line 1475 "hl/src//H5LTparse.c"
    break;

  case 24: /* integer_type: H5T_STD_U16BE_TOKEN  */
#line 137 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U16BE); }
#line 1481 "hl/src//H5LTparse.c"
    break;

  case 25: /* integer_type: H5T_STD_U16LE_TOKEN  */
#line 138 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U16LE); }
#line 1487 "hl/src//H5LTparse.c"
    break;

  case 26: /* integer_type: H5T_STD_U32BE_TOKEN  */
#line 139 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U32BE); }
#line 1493 "hl/src//H5LTparse.c"
    break;

  case 27: /* integer_type: H5T_STD_U32LE_TOKEN  */
#line 140 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U32LE); }
#line 1499 "hl/src//H5LTparse.c"
    break;

  case 28: /* integer_type: H5T_STD_U64BE_TOKEN  */
#line 141 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U64BE); }
#line 1505 "hl/src//H5LTparse.c"
    break;

  case 29: /* integer_type: H5T_STD_U64LE_TOKEN  */
#line 142 "hl/src//H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U64LE); }
#line 1511 "hl/src//H5LTparse.c"
    break;

  case 30: /* integer_type: H5T_NATIVE_CHAR_TOKEN  */
#line 143 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_CHAR); }
#line 1517 "hl/src//H5LTparse.c"
    break;

  case 31: /* integer_type: H5T_NATIVE_SCHAR_TOKEN  */
#line 144 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_SCHAR); }
#line 1523 "hl/src//H5LTparse.c"
    break;

  case 32: /* integer_type: H5T_NATIVE_UCHAR_TOKEN  */
#line 145 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_UCHAR); }
#line 1529 "hl/src//H5LTparse.c"
    break;

  case 33: /* integer_type: H5T_NATIVE_SHORT_TOKEN  */
#line 146 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_SHORT); }
#line 1535 "hl/src//H5LTparse.c"
    break;

  case 34: /* integer_type: H5T_NATIVE_USHORT_TOKEN  */
#line 147 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_USHORT); }
#line 1541 "hl/src//H5LTparse.c"
    break;

  case 35: /* integer_type: H5T_NATIVE_INT_TOKEN  */
#line 148 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_INT); }
#line 1547 "hl/src//H5LTparse.c"
    break;

  case 36: /* integer_type: H5T_NATIVE_UINT_TOKEN  */
#line 149 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_UINT); }
#line 1553 "hl/src//H5LTparse.c"
    break;

  case 37: /* integer_type: H5T_NATIVE_LONG_TOKEN  */
#line 150 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_LONG); }
#line 1559 "hl/src//H5LTparse.c"
    break;

  case 38: /* integer_type: H5T_NATIVE_ULONG_TOKEN  */
#line 151 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_ULONG); }
#line 1565 "hl/src//H5LTparse.c"
    break;

  case 39: /* integer_type: H5T_NATIVE_LLONG_TOKEN  */
#line 152 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_LLONG); }
#line 1571 "hl/src//H5LTparse.c"
    break;

  case 40: /* integer_type: H5T_NATIVE_ULLONG_TOKEN  */
#line 153 "hl/src//H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_ULLONG); }
#line 1577 "hl/src//H5LTparse.c"
    break;

  case 41: /* fp_type: H5T_IEEE_F16BE_TOKEN  */
#line 156 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F16BE); }
#line 1583 "hl/src//H5LTparse.c"
    break;

  case 42: /* fp_type: H5T_IEEE_F16LE_TOKEN  */
#line 157 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F16LE); }
#line 1589 "hl/src//H5LTparse.c"
    break;

  case 43: /* fp_type: H5T_IEEE_F32BE_TOKEN  */
#line 158 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F32BE); }
#line 1595 "hl/src//H5LTparse.c"
    break;

  case 44: /* fp_type: H5T_IEEE_F32LE_TOKEN  */
#line 159 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F32LE); }
#line 1601 "hl/src//H5LTparse.c"
    break;

  case 45: /* fp_type: H5T_IEEE_F64BE_TOKEN  */
#line 160 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F64BE); }
#line 1607 "hl/src//H5LTparse.c"
    break;

  case 46: /* fp_type: H5T_IEEE_F64LE_TOKEN  */
#line 161 "hl/src//H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F64LE); }
#line 1613 "hl/src//H5LTparse.c"
    break;

  case 47: /* fp_type: H5T_FLOAT_BFLOAT16BE_TOKEN  */
#line 162 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_FLOAT_BFLOAT16BE); }
#line 1619 "hl/src//H5LTparse.c"
    break;

  case 48: /* fp_type: H5T_FLOAT_BFLOAT16LE_TOKEN  */
#line 163 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_FLOAT_BFLOAT16LE); }
#line 1625 "hl/src//H5LTparse.c"
    break;

  case 49: /* fp_type: H5T_FLOAT_F8E4M3_TOKEN  */
#line 164 "hl/src//H5LTparse.y"
                                               { (yyval.hid) = H5Tcopy(H5T_FLOAT_F8E4M3); }
#line 1631 "hl/src//H5LTparse.c"
    break;

  case 50: /* fp_type: H5T_FLOAT_F8E5M2_TOKEN  */
#line 165 "hl/src//H5LTparse.y"
                                               { (yyval.hid) = H5Tcopy(H5T_FLOAT_F8E5M2); }
#line 1637 "hl/src//H5LTparse.c"
    break;

  case 51: /* fp_type: H5T_FLOAT_F6E2M3_TOKEN  */
#line 166 "hl/src//H5LTparse.y"
                                               { (yyval.hid) = H5Tcopy(H5T_FLOAT_F6E2M3); }
#line 1643 "hl/src//H5LTparse.c"
    break;

  case 52: /* fp_type: H5T_FLOAT_F6E3M2_TOKEN  */
#line 167 "hl/src//H5LTparse.y"
                                               { (yyval.hid) = H5Tcopy(H5T_FLOAT_F6E3M2); }
#line 1649 "hl/src//H5LTparse.c"
    break;

  case 53: /* fp_type: H5T_NATIVE_FLOAT16_TOKEN  */
#line 168 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_NATIVE_FLOAT16); }
#line 1655 "hl/src//H5LTparse.c"
    break;

  case 54: /* fp_type: H5T_NATIVE_FLOAT_TOKEN  */
#line 169 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_NATIVE_FLOAT); }
#line 1661 "hl/src//H5LTparse.c"
    break;

  case 55: /* fp_type: H5T_NATIVE_DOUBLE_TOKEN  */
#line 170 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_NATIVE_DOUBLE); }
#line 1667 "hl/src//H5LTparse.c"
    break;

  case 56: /* fp_type: H5T_NATIVE_LDOUBLE_TOKEN  */
#line 171 "hl/src//H5LTparse.y"
                                                   { (yyval.hid) = H5Tcopy(H5T_NATIVE_LDOUBLE); }
#line 1673 "hl/src//H5LTparse.c"
    break;

  case 57: /* $@1: %empty  */
#line 175 "hl/src//H5LTparse.y"
                            { csindex++; cmpd_stack[csindex].id = H5Tcreate(H5T_COMPOUND, 1); /*temporarily set size to 1*/ }
#line 1679 "hl/src//H5LTparse.c"
    break;

  case 58: /* compound_type: H5T_COMPOUND_TOKEN $@1 '{' memb_list '}'  */
#line 177 "hl/src//H5LTparse.y"
                            { (yyval.hid) = cmpd_stack[csindex].id; 
                              cmpd_stack[csindex].id = 0;
                              cmpd_stack[csindex].first_memb = 1; 
                              csindex--;
                            }
#line 1689 "hl/src//H5LTparse.c"
    break;

  case 61: /* $@2: %empty  */
#line 186 "hl/src//H5LTparse.y"
                                 { cmpd_stack[csindex].is_field = 1; /*notify lexer a compound member is parsed*/ }
#line 1695 "hl/src//H5LTparse.c"
    break;

  case 62: /* memb_def: ddl_type $@2 field_name field_offset ';'  */
#line 188 "hl/src//H5LTparse.y"
                        {   
                            size_t origin_size, new_size;
                            hid_t dtype_id = cmpd_stack[csindex].id;

                            /*Adjust size and insert member, consider both member size and offset.*/
                            if(cmpd_stack[csindex].first_memb) { /*reclaim the size 1 temporarily set*/
                                new_size = H5Tget_size((yyvsp[-4].hid)) + (yyvsp[-1].ival);
                                H5Tset_size(dtype_id, new_size);
                                /*member name is saved in yylval.sval by lexer*/
                                H5Tinsert(dtype_id, (yyvsp[-2].sval), (yyvsp[-1].ival), (yyvsp[-4].hid));

                                cmpd_stack[csindex].first_memb = 0;
                            } else {
                                origin_size = H5Tget_size(dtype_id);
                                
                                if((yyvsp[-1].ival) == 0) {
                                    new_size = origin_size + H5Tget_size((yyvsp[-4].hid));
                                    H5Tset_size(dtype_id, new_size);
                                    H5Tinsert(dtype_id, (yyvsp[-2].sval), origin_size, (yyvsp[-4].hid));
                                } else {
                                    new_size = (yyvsp[-1].ival) + H5Tget_size((yyvsp[-4].hid));
                                    H5Tset_size(dtype_id, new_size);
                                    H5Tinsert(dtype_id, (yyvsp[-2].sval), (yyvsp[-1].ival), (yyvsp[-4].hid));
                                }
                            }
                            if((yyvsp[-2].sval)) {
                                free((yyvsp[-2].sval));
                                (yyvsp[-2].sval) = NULL;
                            }
                            cmpd_stack[csindex].is_field = 0;
                            H5Tclose((yyvsp[-4].hid));
                             
                            new_size = H5Tget_size(dtype_id);
                        }
#line 1734 "hl/src//H5LTparse.c"
    break;

  case 63: /* field_name: STRING  */
#line 224 "hl/src//H5LTparse.y"
                        {
                            (yyval.sval) = strdup(yylval.sval);
                            free(yylval.sval);
                            yylval.sval = NULL;
                        }
#line 1744 "hl/src//H5LTparse.c"
    break;

  case 64: /* field_offset: %empty  */
#line 231 "hl/src//H5LTparse.y"
                        { (yyval.ival) = 0; }
#line 1750 "hl/src//H5LTparse.c"
    break;

  case 65: /* field_offset: ':' offset  */
#line 233 "hl/src//H5LTparse.y"
                        { (yyval.ival) = yylval.ival; }
#line 1756 "hl/src//H5LTparse.c"
    break;

  case 67: /* $@3: %empty  */
#line 237 "hl/src//H5LTparse.y"
                                        { asindex++; /*pushd onto the stack*/ }
#line 1762 "hl/src//H5LTparse.c"
    break;

  case 68: /* array_type: H5T_ARRAY_TOKEN $@3 '{' dim_list ddl_type '}'  */
#line 239 "hl/src//H5LTparse.y"
                        { 
                          (yyval.hid) = H5Tarray_create2((yyvsp[-1].hid), arr_stack[asindex].ndims, arr_stack[asindex].dims);
                          arr_stack[asindex].ndims = 0;
                          asindex--;
                          H5Tclose((yyvsp[-1].hid));
                        }
#line 1773 "hl/src//H5LTparse.c"
    break;

  case 71: /* $@4: %empty  */
#line 249 "hl/src//H5LTparse.y"
                            { arr_stack[asindex].is_dim = 1; /*notice lexer of dimension size*/ }
#line 1779 "hl/src//H5LTparse.c"
    break;

  case 72: /* $@5: %empty  */
#line 250 "hl/src//H5LTparse.y"
                                { unsigned ndims = arr_stack[asindex].ndims;
                                  arr_stack[asindex].dims[ndims] = (hsize_t)yylval.ival; 
                                  arr_stack[asindex].ndims++;
                                  arr_stack[asindex].is_dim = 0; 
                                }
#line 1789 "hl/src//H5LTparse.c"
    break;

  case 75: /* vlen_type: H5T_VLEN_TOKEN '{' ddl_type '}'  */
#line 261 "hl/src//H5LTparse.y"
                            { (yyval.hid) = H5Tvlen_create((yyvsp[-1].hid)); H5Tclose((yyvsp[-1].hid)); }
#line 1795 "hl/src//H5LTparse.c"
    break;

  case 76: /* complex_type: H5T_NATIVE_FLOAT_COMPLEX_TOKEN  */
#line 264 "hl/src//H5LTparse.y"
                                                        { (yyval.hid) = H5Tcopy(H5T_NATIVE_FLOAT_COMPLEX); }
#line 1801 "hl/src//H5LTparse.c"
    break;

  case 77: /* complex_type: H5T_NATIVE_DOUBLE_COMPLEX_TOKEN  */
#line 265 "hl/src//H5LTparse.y"
                                                         { (yyval.hid) = H5Tcopy(H5T_NATIVE_DOUBLE_COMPLEX); }
#line 1807 "hl/src//H5LTparse.c"
    break;

  case 78: /* complex_type: H5T_NATIVE_LDOUBLE_COMPLEX_TOKEN  */
#line 266 "hl/src//H5LTparse.y"
                                                          { (yyval.hid) = H5Tcopy(H5T_NATIVE_LDOUBLE_COMPLEX); }
#line 1813 "hl/src//H5LTparse.c"
    break;

  case 79: /* complex_type: H5T_COMPLEX_IEEE_F16LE_TOKEN  */
#line 267 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F16LE); }
#line 1819 "hl/src//H5LTparse.c"
    break;

  case 80: /* complex_type: H5T_COMPLEX_IEEE_F16BE_TOKEN  */
#line 268 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F16BE); }
#line 1825 "hl/src//H5LTparse.c"
    break;

  case 81: /* complex_type: H5T_COMPLEX_IEEE_F32LE_TOKEN  */
#line 269 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F32LE); }
#line 1831 "hl/src//H5LTparse.c"
    break;

  case 82: /* complex_type: H5T_COMPLEX_IEEE_F32BE_TOKEN  */
#line 270 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F32BE); }
#line 1837 "hl/src//H5LTparse.c"
    break;

  case 83: /* complex_type: H5T_COMPLEX_IEEE_F64LE_TOKEN  */
#line 271 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F64LE); }
#line 1843 "hl/src//H5LTparse.c"
    break;

  case 84: /* complex_type: H5T_COMPLEX_IEEE_F64BE_TOKEN  */
#line 272 "hl/src//H5LTparse.y"
                                                      { (yyval.hid) = H5Tcopy(H5T_COMPLEX_IEEE_F64BE); }
#line 1849 "hl/src//H5LTparse.c"
    break;

  case 85: /* complex_type: H5T_COMPLEX_TOKEN '{' ddl_type '}'  */
#line 274 "hl/src//H5LTparse.y"
                            { (yyval.hid) = H5Tcomplex_create((yyvsp[-1].hid)); H5Tclose((yyvsp[-1].hid)); }
#line 1855 "hl/src//H5LTparse.c"
    break;

  case 86: /* @6: %empty  */
#line 280 "hl/src//H5LTparse.y"
                            {   
                                size_t size = (size_t)yylval.ival;
                                (yyval.hid) = H5Tcreate(H5T_OPAQUE, size);
                            }
#line 1864 "hl/src//H5LTparse.c"
    break;

  case 87: /* $@7: %empty  */
#line 285 "hl/src//H5LTparse.y"
                            {  
                                H5Tset_tag((yyvsp[-3].hid), yylval.sval);
                                free(yylval.sval);
                                yylval.sval = NULL;
                            }
#line 1874 "hl/src//H5LTparse.c"
    break;

  case 88: /* opaque_type: H5T_OPAQUE_TOKEN '{' OPQ_SIZE_TOKEN opaque_size ';' @6 OPQ_TAG_TOKEN opaque_tag ';' $@7 '}'  */
#line 290 "hl/src//H5LTparse.y"
                            { (yyval.hid) = (yyvsp[-5].hid); }
#line 1880 "hl/src//H5LTparse.c"
    break;

  case 91: /* $@8: %empty  */
#line 299 "hl/src//H5LTparse.y"
                            {  
                                if((yyvsp[-1].ival) == H5T_VARIABLE_TOKEN)
                                    is_variable = 1;
                                else 
                                    str_size = yylval.ival;
                            }
#line 1891 "hl/src//H5LTparse.c"
    break;

  case 92: /* $@9: %empty  */
#line 306 "hl/src//H5LTparse.y"
                            {
                                if((yyvsp[-1].ival) == H5T_STR_NULLTERM_TOKEN)
                                    str_pad = H5T_STR_NULLTERM;
                                else if((yyvsp[-1].ival) == H5T_STR_NULLPAD_TOKEN)
                                    str_pad = H5T_STR_NULLPAD;
                                else if((yyvsp[-1].ival) == H5T_STR_SPACEPAD_TOKEN)
                                    str_pad = H5T_STR_SPACEPAD;
                            }
#line 1904 "hl/src//H5LTparse.c"
    break;

  case 93: /* $@10: %empty  */
#line 315 "hl/src//H5LTparse.y"
                            {  
                                if((yyvsp[-1].ival) == H5T_CSET_ASCII_TOKEN)
                                    str_cset = H5T_CSET_ASCII;
                                else if((yyvsp[-1].ival) == H5T_CSET_UTF8_TOKEN)
                                    str_cset = H5T_CSET_UTF8;
                            }
#line 1915 "hl/src//H5LTparse.c"
    break;

  case 94: /* @11: %empty  */
#line 322 "hl/src//H5LTparse.y"
                            {
                                if((yyvsp[-1].hid) == H5T_C_S1_TOKEN)
                                    (yyval.hid) = H5Tcopy(H5T_C_S1);
                                else if((yyvsp[-1].hid) == H5T_FORTRAN_S1_TOKEN)
                                    (yyval.hid) = H5Tcopy(H5T_FORTRAN_S1);
                            }
#line 1926 "hl/src//H5LTparse.c"
    break;

  case 95: /* string_type: H5T_STRING_TOKEN '{' STRSIZE_TOKEN strsize ';' $@8 STRPAD_TOKEN strpad ';' $@9 CSET_TOKEN cset ';' $@10 CTYPE_TOKEN ctype ';' @11 '}'  */
#line 329 "hl/src//H5LTparse.y"
                            {   
                                hid_t str_id = (yyvsp[-1].hid);

                                /*set string size*/
                                if(is_variable) {
                                    H5Tset_size(str_id, H5T_VARIABLE);
                                    is_variable = 0;
                                } else
                                    H5Tset_size(str_id, str_size);
                                
                                /*set string padding and character set*/
                                H5Tset_strpad(str_id, str_pad);
                                H5Tset_cset(str_id, str_cset);

                                (yyval.hid) = str_id; 
                            }
#line 1947 "hl/src//H5LTparse.c"
    break;

  case 96: /* strsize: H5T_VARIABLE_TOKEN  */
#line 346 "hl/src//H5LTparse.y"
                                               {(yyval.ival) = H5T_VARIABLE_TOKEN;}
#line 1953 "hl/src//H5LTparse.c"
    break;

  case 98: /* strpad: H5T_STR_NULLTERM_TOKEN  */
#line 349 "hl/src//H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_NULLTERM_TOKEN;}
#line 1959 "hl/src//H5LTparse.c"
    break;

  case 99: /* strpad: H5T_STR_NULLPAD_TOKEN  */
#line 350 "hl/src//H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_NULLPAD_TOKEN;}
#line 1965 "hl/src//H5LTparse.c"
    break;

  case 100: /* strpad: H5T_STR_SPACEPAD_TOKEN  */
#line 351 "hl/src//H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_SPACEPAD_TOKEN;}
#line 1971 "hl/src//H5LTparse.c"
    break;

  case 101: /* cset: H5T_CSET_ASCII_TOKEN  */
#line 353 "hl/src//H5LTparse.y"
                                             {(yyval.ival) = H5T_CSET_ASCII_TOKEN;}
#line 1977 "hl/src//H5LTparse.c"
    break;

  case 102: /* cset: H5T_CSET_UTF8_TOKEN  */
#line 354 "hl/src//H5LTparse.y"
                                            {(yyval.ival) = H5T_CSET_UTF8_TOKEN;}
#line 1983 "hl/src//H5LTparse.c"
    break;

  case 103: /* ctype: H5T_C_S1_TOKEN  */
#line 356 "hl/src//H5LTparse.y"
                                               {(yyval.hid) = H5T_C_S1_TOKEN;}
#line 1989 "hl/src//H5LTparse.c"
    break;

  case 104: /* ctype: H5T_FORTRAN_S1_TOKEN  */
#line 357 "hl/src//H5LTparse.y"
                                               {(yyval.hid) = H5T_FORTRAN_S1_TOKEN;}
#line 1995 "hl/src//H5LTparse.c"
    break;

  case 105: /* $@12: %empty  */
#line 361 "hl/src//H5LTparse.y"
                            { is_enum = 1; enum_id = H5Tenum_create((yyvsp[-1].hid)); H5Tclose((yyvsp[-1].hid)); }
#line 2001 "hl/src//H5LTparse.c"
    break;

  case 106: /* enum_type: H5T_ENUM_TOKEN '{' integer_type ';' $@12 enum_list '}'  */
#line 363 "hl/src//H5LTparse.y"
                            { is_enum = 0; /*reset*/ (yyval.hid) = enum_id; }
#line 2007 "hl/src//H5LTparse.c"
    break;

  case 109: /* $@13: %empty  */
#line 368 "hl/src//H5LTparse.y"
                                            {
                                                is_enum_memb = 1; /*indicate member of enum*/
                                                enum_memb_symbol = strdup(yylval.sval); 
                                                free(yylval.sval);
                                                yylval.sval = NULL;
                                            }
#line 2018 "hl/src//H5LTparse.c"
    break;

  case 110: /* enum_def: enum_symbol $@13 enum_val ';'  */
#line 375 "hl/src//H5LTparse.y"
                            {
                                char char_val=(char)yylval.ival;
                                short short_val=(short)yylval.ival;
                                int int_val=(int)yylval.ival;
                                long long_val=(long)yylval.ival;
                                long long llong_val=(long long)yylval.ival;
                                hid_t super = H5Tget_super(enum_id);
                                hid_t native = H5Tget_native_type(super, H5T_DIR_ASCEND);
                                H5T_order_t super_order = H5Tget_order(super);
                                H5T_order_t native_order = H5Tget_order(native);
 
                                if(is_enum && is_enum_memb) { /*if it's an enum member*/
                                    /*To handle machines of different endianness*/
                                    if(H5Tequal(native, H5T_NATIVE_SCHAR) || H5Tequal(native, H5T_NATIVE_UCHAR)) {
                                        if(super_order != native_order)
                                            H5Tconvert(native, super, 1, &char_val, NULL, H5P_DEFAULT); 
                                        H5Tenum_insert(enum_id, enum_memb_symbol, &char_val);
                                    } else if(H5Tequal(native, H5T_NATIVE_SHORT) || H5Tequal(native, H5T_NATIVE_USHORT)) {
                                        if(super_order != native_order)
                                            H5Tconvert(native, super, 1, &short_val, NULL, H5P_DEFAULT); 
                                        H5Tenum_insert(enum_id, enum_memb_symbol, &short_val);
                                    } else if(H5Tequal(native, H5T_NATIVE_INT) || H5Tequal(native, H5T_NATIVE_UINT)) {
                                        if(super_order != native_order)
                                            H5Tconvert(native, super, 1, &int_val, NULL, H5P_DEFAULT); 
                                        H5Tenum_insert(enum_id, enum_memb_symbol, &int_val);
                                    } else if(H5Tequal(native, H5T_NATIVE_LONG) || H5Tequal(native, H5T_NATIVE_ULONG)) {
                                        if(super_order != native_order)
                                            H5Tconvert(native, super, 1, &long_val, NULL, H5P_DEFAULT); 
                                        H5Tenum_insert(enum_id, enum_memb_symbol, &long_val);
                                    } else if(H5Tequal(native, H5T_NATIVE_LLONG) || H5Tequal(native, H5T_NATIVE_ULLONG)) {
                                        if(super_order != native_order)
                                            H5Tconvert(native, super, 1, &llong_val, NULL, H5P_DEFAULT); 
                                        H5Tenum_insert(enum_id, enum_memb_symbol, &llong_val);
                                    }

                                    is_enum_memb = 0; 
                                    if(enum_memb_symbol) free(enum_memb_symbol);
                                }

                                H5Tclose(super);
                                H5Tclose(native);
                            }
#line 2065 "hl/src//H5LTparse.c"
    break;


#line 2069 "hl/src//H5LTparse.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

