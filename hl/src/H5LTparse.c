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
#elif defined __SUNPRO_CC                                         
#pragma disable_warn                                              
#elif defined _MSC_VER                                            
#pragma warning(push, 1)                                          
#endif                                                            
/* A Bison parser, made by GNU Bison 3.7.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.7.2"

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
#line 20 "hl/src/H5LTparse.y"

#include <stdio.h>
#include <string.h>
#include <hdf5.h>

extern int yylex();
extern int yyerror(const char *);

#define STACK_SIZE      16

/*structure for compound type information*/
struct cmpd_info {
    hid_t       id;             /*type ID*/
    hbool_t     is_field;       /*flag to lexer for compound member*/
    hbool_t     first_memb;     /*flag for first compound member*/
};

/*stack for nested compound type*/
struct cmpd_info cmpd_stack[STACK_SIZE] = {
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
    {0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1} };

int csindex = -1;                /*pointer to the top of compound stack*/

/*structure for array type information*/
struct arr_info {
    hsize_t             dims[H5S_MAX_RANK];     /*size of each dimension, limited to 32 dimensions*/
    unsigned            ndims;                  /*number of dimensions*/
    hbool_t             is_dim;                 /*flag to lexer for dimension*/
};
/*stack for nested array type*/
struct arr_info arr_stack[STACK_SIZE];
int asindex = -1;               /*pointer to the top of array stack*/ 

hbool_t     is_str_size = 0;        /*flag to lexer for string size*/
hbool_t     is_str_pad = 0;         /*flag to lexer for string padding*/
H5T_str_t   str_pad;                /*variable for string padding*/
H5T_cset_t  str_cset;               /*variable for string character set*/
hbool_t     is_variable = 0;        /*variable for variable-length string*/
size_t      str_size;               /*variable for string size*/
   
hid_t       enum_id;                /*type ID*/
hbool_t     is_enum = 0;            /*flag to lexer for enum type*/
hbool_t     is_enum_memb = 0;       /*flag to lexer for enum member*/
char*       enum_memb_symbol;       /*enum member symbol string*/

hbool_t is_opq_size = 0;            /*flag to lexer for opaque type size*/
hbool_t is_opq_tag = 0;             /*flag to lexer for opaque type tag*/


#line 131 "hl/src/H5LTparse.c"

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
  YYSYMBOL_H5T_IEEE_F32BE_TOKEN = 30,      /* H5T_IEEE_F32BE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F32LE_TOKEN = 31,      /* H5T_IEEE_F32LE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F64BE_TOKEN = 32,      /* H5T_IEEE_F64BE_TOKEN  */
  YYSYMBOL_H5T_IEEE_F64LE_TOKEN = 33,      /* H5T_IEEE_F64LE_TOKEN  */
  YYSYMBOL_H5T_NATIVE_FLOAT_TOKEN = 34,    /* H5T_NATIVE_FLOAT_TOKEN  */
  YYSYMBOL_H5T_NATIVE_DOUBLE_TOKEN = 35,   /* H5T_NATIVE_DOUBLE_TOKEN  */
  YYSYMBOL_H5T_NATIVE_LDOUBLE_TOKEN = 36,  /* H5T_NATIVE_LDOUBLE_TOKEN  */
  YYSYMBOL_H5T_STRING_TOKEN = 37,          /* H5T_STRING_TOKEN  */
  YYSYMBOL_STRSIZE_TOKEN = 38,             /* STRSIZE_TOKEN  */
  YYSYMBOL_STRPAD_TOKEN = 39,              /* STRPAD_TOKEN  */
  YYSYMBOL_CSET_TOKEN = 40,                /* CSET_TOKEN  */
  YYSYMBOL_CTYPE_TOKEN = 41,               /* CTYPE_TOKEN  */
  YYSYMBOL_H5T_VARIABLE_TOKEN = 42,        /* H5T_VARIABLE_TOKEN  */
  YYSYMBOL_H5T_STR_NULLTERM_TOKEN = 43,    /* H5T_STR_NULLTERM_TOKEN  */
  YYSYMBOL_H5T_STR_NULLPAD_TOKEN = 44,     /* H5T_STR_NULLPAD_TOKEN  */
  YYSYMBOL_H5T_STR_SPACEPAD_TOKEN = 45,    /* H5T_STR_SPACEPAD_TOKEN  */
  YYSYMBOL_H5T_CSET_ASCII_TOKEN = 46,      /* H5T_CSET_ASCII_TOKEN  */
  YYSYMBOL_H5T_CSET_UTF8_TOKEN = 47,       /* H5T_CSET_UTF8_TOKEN  */
  YYSYMBOL_H5T_C_S1_TOKEN = 48,            /* H5T_C_S1_TOKEN  */
  YYSYMBOL_H5T_FORTRAN_S1_TOKEN = 49,      /* H5T_FORTRAN_S1_TOKEN  */
  YYSYMBOL_H5T_OPAQUE_TOKEN = 50,          /* H5T_OPAQUE_TOKEN  */
  YYSYMBOL_OPQ_SIZE_TOKEN = 51,            /* OPQ_SIZE_TOKEN  */
  YYSYMBOL_OPQ_TAG_TOKEN = 52,             /* OPQ_TAG_TOKEN  */
  YYSYMBOL_H5T_COMPOUND_TOKEN = 53,        /* H5T_COMPOUND_TOKEN  */
  YYSYMBOL_H5T_ENUM_TOKEN = 54,            /* H5T_ENUM_TOKEN  */
  YYSYMBOL_H5T_ARRAY_TOKEN = 55,           /* H5T_ARRAY_TOKEN  */
  YYSYMBOL_H5T_VLEN_TOKEN = 56,            /* H5T_VLEN_TOKEN  */
  YYSYMBOL_STRING = 57,                    /* STRING  */
  YYSYMBOL_NUMBER = 58,                    /* NUMBER  */
  YYSYMBOL_59_ = 59,                       /* '{'  */
  YYSYMBOL_60_ = 60,                       /* '}'  */
  YYSYMBOL_61_ = 61,                       /* '['  */
  YYSYMBOL_62_ = 62,                       /* ']'  */
  YYSYMBOL_63_ = 63,                       /* '"'  */
  YYSYMBOL_64_ = 64,                       /* ':'  */
  YYSYMBOL_65_ = 65,                       /* ';'  */
  YYSYMBOL_YYACCEPT = 66,                  /* $accept  */
  YYSYMBOL_start = 67,                     /* start  */
  YYSYMBOL_ddl_type = 68,                  /* ddl_type  */
  YYSYMBOL_atomic_type = 69,               /* atomic_type  */
  YYSYMBOL_integer_type = 70,              /* integer_type  */
  YYSYMBOL_fp_type = 71,                   /* fp_type  */
  YYSYMBOL_compound_type = 72,             /* compound_type  */
  YYSYMBOL_73_1 = 73,                      /* $@1  */
  YYSYMBOL_memb_list = 74,                 /* memb_list  */
  YYSYMBOL_memb_def = 75,                  /* memb_def  */
  YYSYMBOL_76_2 = 76,                      /* $@2  */
  YYSYMBOL_field_name = 77,                /* field_name  */
  YYSYMBOL_field_offset = 78,              /* field_offset  */
  YYSYMBOL_offset = 79,                    /* offset  */
  YYSYMBOL_array_type = 80,                /* array_type  */
  YYSYMBOL_81_3 = 81,                      /* $@3  */
  YYSYMBOL_dim_list = 82,                  /* dim_list  */
  YYSYMBOL_dim = 83,                       /* dim  */
  YYSYMBOL_84_4 = 84,                      /* $@4  */
  YYSYMBOL_85_5 = 85,                      /* $@5  */
  YYSYMBOL_dimsize = 86,                   /* dimsize  */
  YYSYMBOL_vlen_type = 87,                 /* vlen_type  */
  YYSYMBOL_opaque_type = 88,               /* opaque_type  */
  YYSYMBOL_89_6 = 89,                      /* $@6  */
  YYSYMBOL_90_7 = 90,                      /* @7  */
  YYSYMBOL_91_8 = 91,                      /* $@8  */
  YYSYMBOL_92_9 = 92,                      /* $@9  */
  YYSYMBOL_opaque_size = 93,               /* opaque_size  */
  YYSYMBOL_opaque_tag = 94,                /* opaque_tag  */
  YYSYMBOL_string_type = 95,               /* string_type  */
  YYSYMBOL_96_10 = 96,                     /* $@10  */
  YYSYMBOL_97_11 = 97,                     /* $@11  */
  YYSYMBOL_98_12 = 98,                     /* $@12  */
  YYSYMBOL_99_13 = 99,                     /* $@13  */
  YYSYMBOL_100_14 = 100,                   /* @14  */
  YYSYMBOL_strsize = 101,                  /* strsize  */
  YYSYMBOL_strpad = 102,                   /* strpad  */
  YYSYMBOL_cset = 103,                     /* cset  */
  YYSYMBOL_ctype = 104,                    /* ctype  */
  YYSYMBOL_enum_type = 105,                /* enum_type  */
  YYSYMBOL_106_15 = 106,                   /* $@15  */
  YYSYMBOL_enum_list = 107,                /* enum_list  */
  YYSYMBOL_enum_def = 108,                 /* enum_def  */
  YYSYMBOL_109_16 = 109,                   /* $@16  */
  YYSYMBOL_enum_symbol = 110,              /* enum_symbol  */
  YYSYMBOL_enum_val = 111                  /* enum_val  */
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
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
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
#define YYFINAL  58
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   203

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  66
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  95
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  143

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   313


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
       2,     2,     2,     2,    63,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    64,    65,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    61,     2,    62,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    59,     2,    60,     2,     2,     2,     2,
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
      55,    56,    57,    58
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   105,   105,   106,   108,   109,   110,   111,   113,   114,
     115,   116,   117,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     149,   150,   151,   152,   153,   154,   155,   159,   158,   167,
     168,   170,   170,   207,   215,   216,   219,   221,   221,   230,
     231,   233,   234,   233,   241,   244,   250,   251,   256,   257,
     248,   265,   267,   271,   272,   280,   289,   296,   269,   320,
     321,   323,   324,   325,   327,   328,   330,   331,   335,   334,
     339,   340,   342,   342,   396,   398
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
  "H5T_NATIVE_ULLONG_TOKEN", "H5T_IEEE_F32BE_TOKEN",
  "H5T_IEEE_F32LE_TOKEN", "H5T_IEEE_F64BE_TOKEN", "H5T_IEEE_F64LE_TOKEN",
  "H5T_NATIVE_FLOAT_TOKEN", "H5T_NATIVE_DOUBLE_TOKEN",
  "H5T_NATIVE_LDOUBLE_TOKEN", "H5T_STRING_TOKEN", "STRSIZE_TOKEN",
  "STRPAD_TOKEN", "CSET_TOKEN", "CTYPE_TOKEN", "H5T_VARIABLE_TOKEN",
  "H5T_STR_NULLTERM_TOKEN", "H5T_STR_NULLPAD_TOKEN",
  "H5T_STR_SPACEPAD_TOKEN", "H5T_CSET_ASCII_TOKEN", "H5T_CSET_UTF8_TOKEN",
  "H5T_C_S1_TOKEN", "H5T_FORTRAN_S1_TOKEN", "H5T_OPAQUE_TOKEN",
  "OPQ_SIZE_TOKEN", "OPQ_TAG_TOKEN", "H5T_COMPOUND_TOKEN",
  "H5T_ENUM_TOKEN", "H5T_ARRAY_TOKEN", "H5T_VLEN_TOKEN", "STRING",
  "NUMBER", "'{'", "'}'", "'['", "']'", "'\"'", "':'", "';'", "$accept",
  "start", "ddl_type", "atomic_type", "integer_type", "fp_type",
  "compound_type", "$@1", "memb_list", "memb_def", "$@2", "field_name",
  "field_offset", "offset", "array_type", "$@3", "dim_list", "dim", "$@4",
  "$@5", "dimsize", "vlen_type", "opaque_type", "$@6", "@7", "$@8", "$@9",
  "opaque_size", "opaque_tag", "string_type", "$@10", "$@11", "$@12",
  "$@13", "@14", "strsize", "strpad", "cset", "ctype", "enum_type", "$@15",
  "enum_list", "enum_def", "$@16", "enum_symbol", "enum_val", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   123,
     125,    91,    93,    34,    58,    59
};
#endif

#define YYPACT_NINF (-25)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     114,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -24,   -20,   -25,   -15,   -25,
     -14,    49,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,    19,    45,    38,   168,    39,   114,   -25,   -25,
     -25,   -25,    34,   -25,    40,    -4,    43,    56,   -25,    -3,
     -25,   -25,   -25,    37,   -25,    42,   -25,   -25,   -25,   -25,
     -25,    44,   -25,   -25,   -25,    50,   -23,    47,   -25,    64,
      62,    51,   -25,    58,   -25,   -25,   -25,    -2,   -25,   -25,
      89,   -25,    90,    92,   -25,   -25,   -25,    91,    94,    95,
     -25,   -25,   -25,    98,   100,    96,   102,   122,   -25,   103,
     -25,   -25,   -25,   -25,   133,     9,   134,   -25,   -25,   -25,
     135,   -25,   -25,   105,   160,   -25,    46,   -25,   -25,   137,
     -25,   143,   -25
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,     0,     0,    47,     0,    57,
       0,     0,     3,     4,     8,     9,     5,     6,     7,    12,
      10,    11,     0,     0,     0,     0,     0,     0,     1,    73,
      66,    49,     0,    59,     0,     0,     0,     0,    88,     0,
      65,    79,    80,     0,    71,     0,    48,    51,    50,    90,
      61,     0,    60,    74,    67,     0,     0,     0,    58,     0,
       0,     0,    89,     0,    91,    64,    62,     0,    68,    53,
       0,    94,     0,     0,    81,    82,    83,     0,     0,    54,
      92,    63,    75,     0,     0,     0,     0,     0,    72,     0,
      56,    55,    52,    95,     0,     0,     0,    93,    84,    85,
       0,    69,    76,     0,     0,    70,     0,    86,    87,     0,
      77,     0,    78
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -25,   -25,   -21,   -25,   108,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,   -25,
     -25,   -25,   -25,   -25,   -25,   -25
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    41,    42,    43,    44,    45,    46,    54,    67,    78,
      85,   100,   115,   121,    47,    56,    69,    82,    87,   103,
      96,    48,    49,    66,    90,   108,   133,    75,   119,    50,
      65,    89,   117,   134,   141,    73,   107,   130,   139,    51,
      79,    86,    94,   116,   102,   124
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       1,     2,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    52,    64,    92,    71,    53,
      93,   104,   105,   106,    55,    57,    77,    36,    81,    58,
      37,    38,    39,    40,    72,   128,   129,    59,    80,     1,
       2,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,   137,   138,    60,    61,    63,    68,
      70,    74,    83,    97,    88,    95,    36,    84,    99,    37,
      38,    39,    40,    91,    98,   101,    76,     1,     2,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,   109,   110,   111,   118,   112,   113,   120,   114,
     123,   122,   125,    62,    36,   135,   126,    37,    38,    39,
      40,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,   127,   131,
     132,   136,   140,   142
};

static const yytype_int8 yycheck[] =
{
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    59,    57,    60,    42,    59,
      63,    43,    44,    45,    59,    59,    67,    50,    69,     0,
      53,    54,    55,    56,    58,    46,    47,    38,    61,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    48,    49,    51,    59,    59,    65,
      60,    58,    65,    39,    60,    58,    50,    65,    57,    53,
      54,    55,    56,    63,    52,    57,    60,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    63,    63,    62,    57,    65,    63,    58,    64,
      58,    65,    40,    55,    50,    60,    63,    53,    54,    55,
      56,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    65,    65,
      65,    41,    65,    60
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    50,    53,    54,    55,
      56,    67,    68,    69,    70,    71,    72,    80,    87,    88,
      95,   105,    59,    59,    73,    59,    81,    59,     0,    38,
      51,    59,    70,    59,    68,    96,    89,    74,    65,    82,
      60,    42,    58,   101,    58,    93,    60,    68,    75,   106,
      61,    68,    83,    65,    65,    76,   107,    84,    60,    97,
      90,    63,    60,    63,   108,    58,    86,    39,    52,    57,
      77,    57,   110,    85,    43,    44,    45,   102,    91,    63,
      63,    62,    65,    63,    64,    78,   109,    98,    57,    94,
      58,    79,    65,    58,   111,    40,    63,    65,    46,    47,
     103,    65,    65,    92,    99,    60,    41,    48,    49,   104,
      65,   100,    60
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    66,    67,    67,    68,    68,    68,    68,    69,    69,
      69,    69,    69,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    70,
      71,    71,    71,    71,    71,    71,    71,    73,    72,    74,
      74,    76,    75,    77,    78,    78,    79,    81,    80,    82,
      82,    84,    85,    83,    86,    87,    89,    90,    91,    92,
      88,    93,    94,    96,    97,    98,    99,   100,    95,   101,
     101,   102,   102,   102,   103,   103,   104,   104,   106,   105,
     107,   107,   109,   108,   110,   111
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     5,     0,
       2,     0,     7,     1,     0,     2,     1,     0,     6,     0,
       2,     0,     0,     5,     1,     4,     0,     0,     0,     0,
      15,     1,     1,     0,     0,     0,     0,     0,    20,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     7,
       0,     2,     0,     6,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


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

/* This macro is provided for backward compatibility. */
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


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
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
    goto yyexhaustedlab;
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
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
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
#line 105 "hl/src/H5LTparse.y"
                { memset(arr_stack, 0, STACK_SIZE*sizeof(struct arr_info)); /*initialize here?*/ }
#line 1366 "hl/src/H5LTparse.c"
    break;

  case 3: /* start: ddl_type  */
#line 106 "hl/src/H5LTparse.y"
                          { return (yyval.hid);}
#line 1372 "hl/src/H5LTparse.c"
    break;

  case 13: /* integer_type: H5T_STD_I8BE_TOKEN  */
#line 120 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I8BE); }
#line 1378 "hl/src/H5LTparse.c"
    break;

  case 14: /* integer_type: H5T_STD_I8LE_TOKEN  */
#line 121 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I8LE); }
#line 1384 "hl/src/H5LTparse.c"
    break;

  case 15: /* integer_type: H5T_STD_I16BE_TOKEN  */
#line 122 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I16BE); }
#line 1390 "hl/src/H5LTparse.c"
    break;

  case 16: /* integer_type: H5T_STD_I16LE_TOKEN  */
#line 123 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I16LE); }
#line 1396 "hl/src/H5LTparse.c"
    break;

  case 17: /* integer_type: H5T_STD_I32BE_TOKEN  */
#line 124 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I32BE); }
#line 1402 "hl/src/H5LTparse.c"
    break;

  case 18: /* integer_type: H5T_STD_I32LE_TOKEN  */
#line 125 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I32LE); }
#line 1408 "hl/src/H5LTparse.c"
    break;

  case 19: /* integer_type: H5T_STD_I64BE_TOKEN  */
#line 126 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I64BE); }
#line 1414 "hl/src/H5LTparse.c"
    break;

  case 20: /* integer_type: H5T_STD_I64LE_TOKEN  */
#line 127 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_I64LE); }
#line 1420 "hl/src/H5LTparse.c"
    break;

  case 21: /* integer_type: H5T_STD_U8BE_TOKEN  */
#line 128 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U8BE); }
#line 1426 "hl/src/H5LTparse.c"
    break;

  case 22: /* integer_type: H5T_STD_U8LE_TOKEN  */
#line 129 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U8LE); }
#line 1432 "hl/src/H5LTparse.c"
    break;

  case 23: /* integer_type: H5T_STD_U16BE_TOKEN  */
#line 130 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U16BE); }
#line 1438 "hl/src/H5LTparse.c"
    break;

  case 24: /* integer_type: H5T_STD_U16LE_TOKEN  */
#line 131 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U16LE); }
#line 1444 "hl/src/H5LTparse.c"
    break;

  case 25: /* integer_type: H5T_STD_U32BE_TOKEN  */
#line 132 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U32BE); }
#line 1450 "hl/src/H5LTparse.c"
    break;

  case 26: /* integer_type: H5T_STD_U32LE_TOKEN  */
#line 133 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U32LE); }
#line 1456 "hl/src/H5LTparse.c"
    break;

  case 27: /* integer_type: H5T_STD_U64BE_TOKEN  */
#line 134 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U64BE); }
#line 1462 "hl/src/H5LTparse.c"
    break;

  case 28: /* integer_type: H5T_STD_U64LE_TOKEN  */
#line 135 "hl/src/H5LTparse.y"
                                            { (yyval.hid) = H5Tcopy(H5T_STD_U64LE); }
#line 1468 "hl/src/H5LTparse.c"
    break;

  case 29: /* integer_type: H5T_NATIVE_CHAR_TOKEN  */
#line 136 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_CHAR); }
#line 1474 "hl/src/H5LTparse.c"
    break;

  case 30: /* integer_type: H5T_NATIVE_SCHAR_TOKEN  */
#line 137 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_SCHAR); }
#line 1480 "hl/src/H5LTparse.c"
    break;

  case 31: /* integer_type: H5T_NATIVE_UCHAR_TOKEN  */
#line 138 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_UCHAR); }
#line 1486 "hl/src/H5LTparse.c"
    break;

  case 32: /* integer_type: H5T_NATIVE_SHORT_TOKEN  */
#line 139 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_SHORT); }
#line 1492 "hl/src/H5LTparse.c"
    break;

  case 33: /* integer_type: H5T_NATIVE_USHORT_TOKEN  */
#line 140 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_USHORT); }
#line 1498 "hl/src/H5LTparse.c"
    break;

  case 34: /* integer_type: H5T_NATIVE_INT_TOKEN  */
#line 141 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_INT); }
#line 1504 "hl/src/H5LTparse.c"
    break;

  case 35: /* integer_type: H5T_NATIVE_UINT_TOKEN  */
#line 142 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_UINT); }
#line 1510 "hl/src/H5LTparse.c"
    break;

  case 36: /* integer_type: H5T_NATIVE_LONG_TOKEN  */
#line 143 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_LONG); }
#line 1516 "hl/src/H5LTparse.c"
    break;

  case 37: /* integer_type: H5T_NATIVE_ULONG_TOKEN  */
#line 144 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_ULONG); }
#line 1522 "hl/src/H5LTparse.c"
    break;

  case 38: /* integer_type: H5T_NATIVE_LLONG_TOKEN  */
#line 145 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_LLONG); }
#line 1528 "hl/src/H5LTparse.c"
    break;

  case 39: /* integer_type: H5T_NATIVE_ULLONG_TOKEN  */
#line 146 "hl/src/H5LTparse.y"
                                                { (yyval.hid) = H5Tcopy(H5T_NATIVE_ULLONG); }
#line 1534 "hl/src/H5LTparse.c"
    break;

  case 40: /* fp_type: H5T_IEEE_F32BE_TOKEN  */
#line 149 "hl/src/H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F32BE); }
#line 1540 "hl/src/H5LTparse.c"
    break;

  case 41: /* fp_type: H5T_IEEE_F32LE_TOKEN  */
#line 150 "hl/src/H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F32LE); }
#line 1546 "hl/src/H5LTparse.c"
    break;

  case 42: /* fp_type: H5T_IEEE_F64BE_TOKEN  */
#line 151 "hl/src/H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F64BE); }
#line 1552 "hl/src/H5LTparse.c"
    break;

  case 43: /* fp_type: H5T_IEEE_F64LE_TOKEN  */
#line 152 "hl/src/H5LTparse.y"
                                             { (yyval.hid) = H5Tcopy(H5T_IEEE_F64LE); }
#line 1558 "hl/src/H5LTparse.c"
    break;

  case 44: /* fp_type: H5T_NATIVE_FLOAT_TOKEN  */
#line 153 "hl/src/H5LTparse.y"
                                                  { (yyval.hid) = H5Tcopy(H5T_NATIVE_FLOAT); }
#line 1564 "hl/src/H5LTparse.c"
    break;

  case 45: /* fp_type: H5T_NATIVE_DOUBLE_TOKEN  */
#line 154 "hl/src/H5LTparse.y"
                                                  { (yyval.hid) = H5Tcopy(H5T_NATIVE_DOUBLE); }
#line 1570 "hl/src/H5LTparse.c"
    break;

  case 46: /* fp_type: H5T_NATIVE_LDOUBLE_TOKEN  */
#line 155 "hl/src/H5LTparse.y"
                                                  { (yyval.hid) = H5Tcopy(H5T_NATIVE_LDOUBLE); }
#line 1576 "hl/src/H5LTparse.c"
    break;

  case 47: /* $@1: %empty  */
#line 159 "hl/src/H5LTparse.y"
                            { csindex++; cmpd_stack[csindex].id = H5Tcreate(H5T_COMPOUND, 1); /*temporarily set size to 1*/ }
#line 1582 "hl/src/H5LTparse.c"
    break;

  case 48: /* compound_type: H5T_COMPOUND_TOKEN $@1 '{' memb_list '}'  */
#line 161 "hl/src/H5LTparse.y"
                            { (yyval.hid) = cmpd_stack[csindex].id; 
                              cmpd_stack[csindex].id = 0;
                              cmpd_stack[csindex].first_memb = 1; 
                              csindex--;
                            }
#line 1592 "hl/src/H5LTparse.c"
    break;

  case 51: /* $@2: %empty  */
#line 170 "hl/src/H5LTparse.y"
                                 { cmpd_stack[csindex].is_field = 1; /*notify lexer a compound member is parsed*/ }
#line 1598 "hl/src/H5LTparse.c"
    break;

  case 52: /* memb_def: ddl_type $@2 '"' field_name '"' field_offset ';'  */
#line 172 "hl/src/H5LTparse.y"
                        {   
                            size_t origin_size, new_size;
                            hid_t dtype_id = cmpd_stack[csindex].id;

                            /*Adjust size and insert member, consider both member size and offset.*/
                            if(cmpd_stack[csindex].first_memb) { /*reclaim the size 1 temporarily set*/
                                new_size = H5Tget_size((yyvsp[-6].hid)) + (yyvsp[-1].ival);
                                H5Tset_size(dtype_id, new_size);
                                /*member name is saved in yylval.sval by lexer*/
                                H5Tinsert(dtype_id, (yyvsp[-3].sval), (yyvsp[-1].ival), (yyvsp[-6].hid));

                                cmpd_stack[csindex].first_memb = 0;
                            } else {
                                origin_size = H5Tget_size(dtype_id);
                                
                                if((yyvsp[-1].ival) == 0) {
                                    new_size = origin_size + H5Tget_size((yyvsp[-6].hid));
                                    H5Tset_size(dtype_id, new_size);
                                    H5Tinsert(dtype_id, (yyvsp[-3].sval), origin_size, (yyvsp[-6].hid));
                                } else {
                                    new_size = (yyvsp[-1].ival) + H5Tget_size((yyvsp[-6].hid));
                                    H5Tset_size(dtype_id, new_size);
                                    H5Tinsert(dtype_id, (yyvsp[-3].sval), (yyvsp[-1].ival), (yyvsp[-6].hid));
                                }
                            }
                            if((yyvsp[-3].sval)) {
                                free((yyvsp[-3].sval));
                                (yyvsp[-3].sval) = NULL;
                            }
                            cmpd_stack[csindex].is_field = 0;
                            H5Tclose((yyvsp[-6].hid));
                             
                            new_size = H5Tget_size(dtype_id);
                        }
#line 1637 "hl/src/H5LTparse.c"
    break;

  case 53: /* field_name: STRING  */
#line 208 "hl/src/H5LTparse.y"
                        {
                            (yyval.sval) = strdup(yylval.sval);
                            free(yylval.sval);
                            yylval.sval = NULL;
                        }
#line 1647 "hl/src/H5LTparse.c"
    break;

  case 54: /* field_offset: %empty  */
#line 215 "hl/src/H5LTparse.y"
                        { (yyval.ival) = 0; }
#line 1653 "hl/src/H5LTparse.c"
    break;

  case 55: /* field_offset: ':' offset  */
#line 217 "hl/src/H5LTparse.y"
                        { (yyval.ival) = yylval.ival; }
#line 1659 "hl/src/H5LTparse.c"
    break;

  case 57: /* $@3: %empty  */
#line 221 "hl/src/H5LTparse.y"
                                        { asindex++; /*pushd onto the stack*/ }
#line 1665 "hl/src/H5LTparse.c"
    break;

  case 58: /* array_type: H5T_ARRAY_TOKEN $@3 '{' dim_list ddl_type '}'  */
#line 223 "hl/src/H5LTparse.y"
                        { 
                          (yyval.hid) = H5Tarray_create2((yyvsp[-1].hid), arr_stack[asindex].ndims, arr_stack[asindex].dims);
                          arr_stack[asindex].ndims = 0;
                          asindex--;
                          H5Tclose((yyvsp[-1].hid));
                        }
#line 1676 "hl/src/H5LTparse.c"
    break;

  case 61: /* $@4: %empty  */
#line 233 "hl/src/H5LTparse.y"
                            { arr_stack[asindex].is_dim = 1; /*notice lexer of dimension size*/ }
#line 1682 "hl/src/H5LTparse.c"
    break;

  case 62: /* $@5: %empty  */
#line 234 "hl/src/H5LTparse.y"
                                { unsigned ndims = arr_stack[asindex].ndims;
                                  arr_stack[asindex].dims[ndims] = (hsize_t)yylval.ival; 
                                  arr_stack[asindex].ndims++;
                                  arr_stack[asindex].is_dim = 0; 
                                }
#line 1692 "hl/src/H5LTparse.c"
    break;

  case 65: /* vlen_type: H5T_VLEN_TOKEN '{' ddl_type '}'  */
#line 245 "hl/src/H5LTparse.y"
                            { (yyval.hid) = H5Tvlen_create((yyvsp[-1].hid)); H5Tclose((yyvsp[-1].hid)); }
#line 1698 "hl/src/H5LTparse.c"
    break;

  case 66: /* $@6: %empty  */
#line 250 "hl/src/H5LTparse.y"
                                           { is_opq_size = 1; }
#line 1704 "hl/src/H5LTparse.c"
    break;

  case 67: /* @7: %empty  */
#line 251 "hl/src/H5LTparse.y"
                            {   
                                size_t size = (size_t)yylval.ival;
                                (yyval.hid) = H5Tcreate(H5T_OPAQUE, size);
                                is_opq_size = 0;    
                            }
#line 1714 "hl/src/H5LTparse.c"
    break;

  case 68: /* $@8: %empty  */
#line 256 "hl/src/H5LTparse.y"
                                          { is_opq_tag = 1; }
#line 1720 "hl/src/H5LTparse.c"
    break;

  case 69: /* $@9: %empty  */
#line 257 "hl/src/H5LTparse.y"
                            {  
                                H5Tset_tag((yyvsp[-6].hid), yylval.sval);
                                free(yylval.sval);
                                yylval.sval = NULL;
                                is_opq_tag = 0;
                            }
#line 1731 "hl/src/H5LTparse.c"
    break;

  case 70: /* opaque_type: H5T_OPAQUE_TOKEN '{' OPQ_SIZE_TOKEN $@6 opaque_size ';' @7 OPQ_TAG_TOKEN $@8 '"' opaque_tag '"' ';' $@9 '}'  */
#line 263 "hl/src/H5LTparse.y"
                            { (yyval.hid) = (yyvsp[-8].hid); }
#line 1737 "hl/src/H5LTparse.c"
    break;

  case 73: /* $@10: %empty  */
#line 271 "hl/src/H5LTparse.y"
                                          { is_str_size = 1; }
#line 1743 "hl/src/H5LTparse.c"
    break;

  case 74: /* $@11: %empty  */
#line 272 "hl/src/H5LTparse.y"
                            {  
                                if((yyvsp[-1].ival) == H5T_VARIABLE_TOKEN)
                                    is_variable = 1;
                                else 
                                    str_size = yylval.ival;
                                is_str_size = 0; 
                            }
#line 1755 "hl/src/H5LTparse.c"
    break;

  case 75: /* $@12: %empty  */
#line 280 "hl/src/H5LTparse.y"
                            {
                                if((yyvsp[-1].ival) == H5T_STR_NULLTERM_TOKEN)
                                    str_pad = H5T_STR_NULLTERM;
                                else if((yyvsp[-1].ival) == H5T_STR_NULLPAD_TOKEN)
                                    str_pad = H5T_STR_NULLPAD;
                                else if((yyvsp[-1].ival) == H5T_STR_SPACEPAD_TOKEN)
                                    str_pad = H5T_STR_SPACEPAD;
                            }
#line 1768 "hl/src/H5LTparse.c"
    break;

  case 76: /* $@13: %empty  */
#line 289 "hl/src/H5LTparse.y"
                            {  
                                if((yyvsp[-1].ival) == H5T_CSET_ASCII_TOKEN)
                                    str_cset = H5T_CSET_ASCII;
                                else if((yyvsp[-1].ival) == H5T_CSET_UTF8_TOKEN)
                                    str_cset = H5T_CSET_UTF8;
                            }
#line 1779 "hl/src/H5LTparse.c"
    break;

  case 77: /* @14: %empty  */
#line 296 "hl/src/H5LTparse.y"
                            {
                                if((yyvsp[-1].hid) == H5T_C_S1_TOKEN)
                                    (yyval.hid) = H5Tcopy(H5T_C_S1);
                                else if((yyvsp[-1].hid) == H5T_FORTRAN_S1_TOKEN)
                                    (yyval.hid) = H5Tcopy(H5T_FORTRAN_S1);
                            }
#line 1790 "hl/src/H5LTparse.c"
    break;

  case 78: /* string_type: H5T_STRING_TOKEN '{' STRSIZE_TOKEN $@10 strsize ';' $@11 STRPAD_TOKEN strpad ';' $@12 CSET_TOKEN cset ';' $@13 CTYPE_TOKEN ctype ';' @14 '}'  */
#line 303 "hl/src/H5LTparse.y"
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
#line 1811 "hl/src/H5LTparse.c"
    break;

  case 79: /* strsize: H5T_VARIABLE_TOKEN  */
#line 320 "hl/src/H5LTparse.y"
                                               {(yyval.ival) = H5T_VARIABLE_TOKEN;}
#line 1817 "hl/src/H5LTparse.c"
    break;

  case 81: /* strpad: H5T_STR_NULLTERM_TOKEN  */
#line 323 "hl/src/H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_NULLTERM_TOKEN;}
#line 1823 "hl/src/H5LTparse.c"
    break;

  case 82: /* strpad: H5T_STR_NULLPAD_TOKEN  */
#line 324 "hl/src/H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_NULLPAD_TOKEN;}
#line 1829 "hl/src/H5LTparse.c"
    break;

  case 83: /* strpad: H5T_STR_SPACEPAD_TOKEN  */
#line 325 "hl/src/H5LTparse.y"
                                               {(yyval.ival) = H5T_STR_SPACEPAD_TOKEN;}
#line 1835 "hl/src/H5LTparse.c"
    break;

  case 84: /* cset: H5T_CSET_ASCII_TOKEN  */
#line 327 "hl/src/H5LTparse.y"
                                             {(yyval.ival) = H5T_CSET_ASCII_TOKEN;}
#line 1841 "hl/src/H5LTparse.c"
    break;

  case 85: /* cset: H5T_CSET_UTF8_TOKEN  */
#line 328 "hl/src/H5LTparse.y"
                                            {(yyval.ival) = H5T_CSET_UTF8_TOKEN;}
#line 1847 "hl/src/H5LTparse.c"
    break;

  case 86: /* ctype: H5T_C_S1_TOKEN  */
#line 330 "hl/src/H5LTparse.y"
                                               {(yyval.hid) = H5T_C_S1_TOKEN;}
#line 1853 "hl/src/H5LTparse.c"
    break;

  case 87: /* ctype: H5T_FORTRAN_S1_TOKEN  */
#line 331 "hl/src/H5LTparse.y"
                                               {(yyval.hid) = H5T_FORTRAN_S1_TOKEN;}
#line 1859 "hl/src/H5LTparse.c"
    break;

  case 88: /* $@15: %empty  */
#line 335 "hl/src/H5LTparse.y"
                            { is_enum = 1; enum_id = H5Tenum_create((yyvsp[-1].hid)); H5Tclose((yyvsp[-1].hid)); }
#line 1865 "hl/src/H5LTparse.c"
    break;

  case 89: /* enum_type: H5T_ENUM_TOKEN '{' integer_type ';' $@15 enum_list '}'  */
#line 337 "hl/src/H5LTparse.y"
                            { is_enum = 0; /*reset*/ (yyval.hid) = enum_id; }
#line 1871 "hl/src/H5LTparse.c"
    break;

  case 92: /* $@16: %empty  */
#line 342 "hl/src/H5LTparse.y"
                                            {
                                                is_enum_memb = 1; /*indicate member of enum*/
#ifdef H5_HAVE_WIN32_API
                                                enum_memb_symbol = _strdup(yylval.sval); 
#else /* H5_HAVE_WIN32_API */
                                                enum_memb_symbol = strdup(yylval.sval); 
#endif  /* H5_HAVE_WIN32_API */
                                                free(yylval.sval);
                                                yylval.sval = NULL;
                                            }
#line 1886 "hl/src/H5LTparse.c"
    break;

  case 93: /* enum_def: '"' enum_symbol '"' $@16 enum_val ';'  */
#line 353 "hl/src/H5LTparse.y"
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
#line 1933 "hl/src/H5LTparse.c"
    break;


#line 1937 "hl/src/H5LTparse.c"

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
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
yyreturn:
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

