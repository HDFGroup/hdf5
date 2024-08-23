/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *
 * Data and structure definitions for h5import
 *
 */

#ifndef H5IMPORT_H
#define H5IMPORT_H

/** \page H5TOOL_IM_UG The HDF5 h5import Tool
 *
 * \section sec_cltools_h5import h5import
 *
 * \subsection subsec_cltools_h5import_intro Introduction
 *  With h5import, you can convert data stored in one or more ASCII or binary files
 *     into one or more datasets (in accordance with the
 *     user-specified type and storage properties) in an existing
 *     or new HDF5 file.
 *
 * \subsection subsec_cltools_h5import_desc Description
 *  The primary objective of the utility is to convert floating
 *     point or integer data stored in \b ASCII text or binary form
 *     into a dataset according to the type and storage properties
 *     specified by the user. The utility can also accept \b ASCII
 *     text files and store the contents in a compact form as an
 *     array of one-dimensional strings.
 *
 *     The input data to be written as a dataset can be provided
 *     to the utility in one of the following forms:
 *     \li 1. ASCII text file with numeric data (floating point or
 *     integer data)
 *     \li 2. Binary file with native floating point data (32-bit or
 *     64-bit)
 *     \li 3. Binary file with native integer (signed or unsigned)
 *     data (8-bit or 16-bit or 32-bit or 64-bit)
 *     \li 4. ASCII text file containing strings (text data)
 *
 *     Every input file is associated with a configuration file
 *     also provided as an input to the utility. (See Section
 *     \ref subsec_cltools_h5import_config to know how it is to be organized).
 *     The class, size and dimensions of the input data is
 *     specified in this configuration file. A point to note is
 *     that the floating point data in the \b ASCII text file may be
 *     organized in the fixed floating form (for example 323.56)
 *     or in scientific notation (for example 3.23E+02). A
 *     different input-class specification is to be used for both forms.
 *
 *     The utility extracts the input data from the input file
 *     according to the specified parameters and saves it into
 *     an HDF5 dataset.
 *
 *     The user can specify output type and storage properties in
 *     the configuration file. The user is required to specify the
 *     path of the dataset. If the groups in the path leading to
 *     the dataset do not exist, the groups will be created by the
 *     utility. If no group is specified, the dataset will be
 *     created under the root group.
 *
 *     In addition to the name, the user is also required to
 *     provide the class and size of output data to be written to
 *     the dataset and may optionally specify the output-architecture,
 *     and the output-byte-order. If output-architecture is not
 *     specified, the default is \b NATIVE. Output-byte-orders are fixed
 *     for some architectures and may be specified only if output-
 *     architecture is \b IEEE, \b UNIX or \b STD.
 *
 *     Also, layout and other storage properties such as
 *     compression, external storage and extendible datasets may be
 *     optionally specified.  The layout and storage properties
 *     denote how raw data is to be organized on the disk. If these
 *     options are not specified, the default is \b Contiguous layout
 *     and storage.
 *
 *     The dataset can be organized in any of the following ways:
 *     \li 1. <strong>Contiguous</strong>
 *     \li 2. <strong>Chunked</strong>
 *     \li 3. <strong>External Storage File</strong> (has to be contiguous)
 *     \li 4. <strong>Extendible data sets</strong>(has to be chunked)
 *     \li 5. <strong>Compressed </strong> (has to be chunked)
 *     \li 6. <strong>Compressed & Extendible</strong> (has to be chunked)
 *
 *     If the user wants to store raw data in a non-HDF5 file then
 *     the external storage file option is to be used and the name
 *     of the file is to be specified.
 *
 *     If the user wants the dimensions of the dataset to be
 *     unlimited, the extendible data set option can be chosen.
 *
 *     The user may also specify the type of compression and the
 *     level to which the data set must be compressed by setting
 *     the compressed option.
 *
 * \subsection subsec_cltools_h5import_usage Usage
 *    <h4>h5import -h[elp], OR h5import \<infile\> -c[onfig] \<configfile\> [\<infile\>
 * -c[config]\<confile2\>...] -o[utfile] \<outfile\></h4>
 *
 * \subsection subsec_cltools_h5import_help Help
 * \li <strong>-h[elp]</strong>  Print a usage message and exit
 *
 * \subsubsection subsubsec_cltools_h5import_options Program Options
 * \li <strong>\<infile(s)\></strong>
 *   Name of the Input file(s), containing a
 *   single n-dimensional floating point or integer array
 *   in either ASCII text, native floating point(32-bit
 *   or 64-bit) or native integer(8-bit or 16-bit or
 *   32-bit or 64-bit). Data to be specified in the order
 *   of fastest changing dimensions first.
 *
 * \li <strong>-c[config] \<configfile\></strong>
 *   Every input file should be associated with a
 *   configuration file and this is done by the -c option.
 *   \<configfile\> is the name of the configuration file.
 *   (See Section \ref subsec_cltools_h5import_config).
 *
 * \li <strong>-o[utfile] \<outfile\></strong>
 *   Name of the HDF5 output file. Data from one or more
 *   input files are stored as one or more data sets in
 *   \<outfile\>. The output file may be an existing file or
 *   it may be new, in which case it will be created.
 *
 * \subsection subsec_cltools_h5import_config Configuration File
 *   The configuration file is an ASCII text file and must be
 *   the ddl formatted file (without data values) produced by \b h5dump
 *   when used with the options \code -o outfilename -b \endcode of a single dataset (-d)
 *   OR organized as <strong>CONFIG-KEYWORD VALUE</strong> pairs, one pair on each
 *   line.
 *
 *   The configuration file may have the following keywords each
 *   followed by an acceptable value.
 *
 * \subsubsection subsubsec_cltools_h5import_config_req Required KEYWORDS
 * \li <strong>PATH</strong>
 * \li <strong>INPUT-CLASS</strong>
 * \li <strong>INPUT-SIZE</strong>
 * \li <strong>INPUT-BYTE-ORDER</strong>
 * \li <strong>RANK</strong>
 * \li <strong>DIMENSION-SIZES</strong>
 * \li <strong>OUTPUT-CLASS</strong>
 * \li <strong>OUTPUT-SIZE</strong>
 *
 * \subsubsection subsubsec_cltools_h5import_config_opt Optional KEYWORDS
 * \li <strong>OUTPUT-ARCHITECTURE</strong>
 * \li <strong>OUTPUT-BYTE-ORDER</strong>
 * \li <strong>CHUNKED-DIMENSION-SIZES</strong>
 * \li <strong>COMPRESSION-TYPE</strong>
 * \li <strong>COMPRESSION-PARAM</strong>
 * \li <strong>EXTERNAL-STORAGE</strong>
 * \li <strong>MAXIMUM-DIMENSIONS</strong>
 *
 * \subsubsection subsubsec_cltools_h5import_config_val Values for keywords
 * \li <strong>PATH</strong>
 *     Strings separated by spaces to represent
 *     the path of the dataset. If the groups in
 *     the path do not exist, they will be created.
 *     For example,
 *       <ul><li>PATH grp1/grp2/dataset1</li>
 *       <li>PATH: keyword</li>
 *       <li>grp1: group under the root. If non-existent will be created</li>
 *       <li>grp2: group under grp1. If non-existent will be created under grp1</li>
 *       <li>dataset1: the name of the dataset to be created</li></ul>
 *
 * \li <strong>INPUT-CLASS</strong>
 *     String denoting the type of input data.
 *     <ul><li>TEXTIN</li>
 *     <li>TEXTFP</li>
 *     <li>FP</li>
 *     <li>IN</li>
 *     <li>STR</li>
 *     <li>TEXTUIN</li>
 *     <li>UIN</li></ul>
 *     \b INPUT-CLASS "TEXTIN" denotes an ASCII text file with signed integer data in ASCII form,
 *     \b INPUT-CLASS "TEXTUIN" denotes an ASCII text file with unsigned integer data in ASCII form,
 *     "TEXTFP" denotes an ASCII text file containing floating point data in the fixed notation
 *     (325.34), <br />
 *     "FP" denotes a floating point binary file,
 *     "IN" denotes a signed integer binary file,
 *     "UIN" denotes an unsigned integer binary file,
 *      & "STR" denotes an ASCII text file the contents of which should be stored as a 1-D
 *     array of strings.<br />
 *     If \b INPUT-CLASS is "STR", then \b RANK,
 *     \b DIMENSION-SIZES, \b OUTPUT-CLASS, \b OUTPUT-SIZE,
 *     \b OUTPUT-ARCHITECTURE and \b OUTPUT-BYTE-ORDER
 *     will be ignored.
 *
 * \li <strong>INPUT-SIZE</strong>
 *     Integer denoting the size of the input data (8, 16, 32, 64).
 *     <ul><li>For floating point, \b INPUT-SIZE can be 32 or 64.</li>
 *     <li>For integers (signed and unsigned) \b INPUT-SIZE can be 8, 16, 32 or 64.</li></ul>
 *
 * \li <strong>RANK</strong>
 *     Integer denoting the number of dimensions.
 *
 * \li <strong>DIMENSION-SIZES</strong>
 *     Integers separated by spaces to denote the dimension sizes for the number of dimensions
 *     determined by rank.
 *
 * \li <strong>OUTPUT-CLASS</strong>
 *     String denoting data type of the dataset to be written ("IN","FP", "UIN")
 *
 * \li <strong>OUTPUT-SIZE</strong>
 *     Integer denoting the size of the data in the output dataset to be written.
 *     If \b OUTPUT-CLASS is "FP", \b OUTPUT-SIZE can be 32 or 64.
 *     If \b OUTPUT-CLASS is "IN" or "UIN", \b OUTPUT-SIZE can be 8, 16, 32 or 64.
 *
 * \li <strong>OUTPUT-ARCHITECTURE</strong>
 *     \b STRING denoting the type of output architecture. Can accept the following values
 *     <ul><li>STD</li>
 *     <li>IEEE</li>
 *     <li>INTEL</li>
 *     <li>CRAY</li>
 *     <li>MIPS</li>
 *     <li>ALPHA</li>
 *     <li>NATIVE (default)</li>
 *     <li>UNIX</li></ul>
 *
 * \li <strong>OUTPUT-BYTE-ORDER</strong>
 *     String denoting the output-byte-order. Ignored if the \b OUTPUT-ARCHITECTURE is not specified or
 *     if it is \b IEEE, \b UNIX or \b STD. Can accept the following values.
 *     <ul><li>BE (default)</li>
 *     <li>LE</li></ul>
 *
 * \li <strong>CHUNKED-DIMENSION-SIZES</strong>
 *     Integers separated by spaces to denote the dimension sizes of the chunk for the number of
 *     dimensions determined by rank. Required field to denote that the dataset will be stored with
 *     chunked storage. If this field is absent the dataset will be stored with contiguous storage.
 *
 * \li <strong>COMPRESSION-TYPE</strong>
 *     String denoting the type of compression to be used with the chunked storage. Requires the
 *     \b CHUNKED-DIMENSION-SIZES to be specified. The only currently supported compression method is \b GZIP.
 *     Will accept the following value
 *     <ul><li>GZIP</li></ul>
 *
 * \li <strong>COMPRESSION-PARAM</strong>
 *     Integer used to denote compression level and this option is to be always specified when
 *     the \b COMPRESSION-TYPE option is specified. The values are applicable only to \b GZIP
 *     compression.<br />
 *     Value 1-9: The level of Compression.<br />
 *     1 will result in the fastest compression while 9 will result in
 *     the best compression ratio.<br />
 *     The default level of compression is 6.
 *
 * \li <strong>EXTERNAL-STORAGE</strong>
 *     String to denote the name of the non-HDF5 file to store data to. Cannot be used if \b
 * CHUNKED-DIMENSIONS or \b COMPRESSION-TYPE or \b EXTENDIBLE-DATASET is specified. Value
 * \<external-filename\>: the name of the external file as a string to be used.
 *
 * \li <strong>MAXIMUM-DIMENSIONS</strong>
 *     Integers separated by spaces to denote the maximum dimension sizes of all the
 *     dimensions determined by rank. Requires the \b CHUNKED-DIMENSION-SIZES to be specified. A value of
 *     -1 for any dimension implies \b UNLIMITED \b DIMENSION size for that particular dimension.
 *
 * \subsection subsec_cltools_h5import_examples Usage Examples
 * \li <strong>Configuration File may look like</strong>
 * \code
 *           PATH work h5 pkamat First-set
 *           INPUT-CLASS TEXTFP
 *           RANK 3
 *           DIMENSION-SIZES 5 2 4
 *           OUTPUT-CLASS FP
 *           OUTPUT-SIZE 64
 *           OUTPUT-ARCHITECTURE IEEE
 *           OUTPUT-BYTE-ORDER LE
 *           CHUNKED-DIMENSION-SIZES 2 2 2
 * \endcode
 *     The above configuration will accept a floating point array
 *     (5 x 2 x 4) in an ASCII file with the rank and dimension sizes
 *     specified and will save it in a chunked dataset (of pattern
 *     2 X 2 X 2) of 64-bit floating point in the little-endian order
 *     and IEEE architecture. The dataset will be stored at
 *     "/work/h5/pkamat/First-set"
 *
 * \li <strong>Another configuration could be</strong>
 * \code
 *           PATH Second-set
 *           INPUT-CLASS IN
 *           RANK 5
 *           DIMENSION-SIZES 6 3 5 2 4
 *           OUTPUT-CLASS IN
 *           OUTPUT-SIZE 32
 *           CHUNKED-DIMENSION-SIZES 2 2 2 2 2
 *           EXTENDIBLE-DATASET 1 3
 *            COMPRESSION-TYPE GZIP
 *           COMPRESSION-PARAM 7
 * \endcode
 *      The above configuration will accept an integer array
 *      (6 X 3 X 5 x 2 x 4) in a binary file with the rank and
 *      dimension sizes specified and will save it in a chunked dataset
 *      (of pattern 2 X 2 X 2 X 2 X 2) of 32-bit floating point in
 *      native format (as output-architecture is not specified). The
 *      first and the third dimension will be defined as unlimited. The
 *      dataset will be compressed using GZIP and a compression level
 *      of 7.<br />
 *      The dataset will be stored at \code /Second-set \endcode
 *
 *
 */

/*
 * state table tokens
 */
#define FILNAME 0
/* filename */
#define OPT_o 1
/* output filename */
#define OPT_c         2  /* configuration filename */
#define OPT_h         3  /* request for explanation */
#define OPT_d         4  /* dimensions */
#define OPT_p         5  /* pathname */
#define OPT_t         6  /* data type */
#define OPT_s         7  /* data size */
#define INVALID_TOKEN 20 /* invalid token */

#define MAX_GROUPS_IN_PATH   20
#define MAX_PATH_NAME_LENGTH 255
#define NUM_KEYS             15
#define MIN_NUM_DIMENSION    1
#define MAX_NUM_DIMENSION    32
#define BASE_10              10

#define PATH           0
#define INPUT_CLASS    1
#define INPUT_SIZE     2
#define RANK           3
#define DIM            4
#define OUTPUT_CLASS   5
#define OUTPUT_SIZE    6
#define OUTPUT_ARCH    7
#define OUTPUT_B_ORDER 8
#define CHUNK          9
#define COMPRESS       10
#define COMPRESS_PARAM 11
#define EXTERNALSTORE  12
#define EXTEND         13
#define INPUT_B_ORDER  14

/* data types */
#define H5DT_INT8    signed char
#define H5DT_INT16   short
#define H5DT_INT32   int
#define H5DT_FLOAT32 float
#define H5DT_FLOAT64 double
#define VOIDP        void *
#define H5DT_UINT8   unsigned char
#define H5DT_UINT16  unsigned short
#define H5DT_UINT32  unsigned int
#define H5DT_INT64   long long
#define H5DT_UINT64  unsigned H5DT_INT64

struct path_info {
    char group[MAX_GROUPS_IN_PATH][MAX_PATH_NAME_LENGTH];
    int  count;
};

struct Input {
    int              h5dumpInput;
    struct path_info path;
    int              inputClass;
    int              inputSize;
    int              inputArchitecture;
    int              inputByteOrder;
    int              rank;
    hsize_t         *sizeOfDimension;
    int              outputClass;
    int              outputSize;
    int              outputArchitecture;
    int              outputByteOrder;
    hsize_t         *sizeOfChunk;
    hsize_t         *maxsizeOfDimension;
    int              compressionType;
    int              compressionParam;
    char            *externFilename;
    VOIDP            data;
    int              configOptionVector[NUM_KEYS];
};

struct infilesformat {
    char         datafile[MAX_PATH_NAME_LENGTH];
    char         configfile[MAX_PATH_NAME_LENGTH];
    struct Input in;
    int          config; /* Configfile present? No - 0. Yes - 1 */
};

struct Options {
    struct infilesformat infiles[30];  /* structure to hold the list of input file names. Limited to 30*/
    char                 outfile[256]; /* output file name */
    int                  fcount;       /* number of input files */
};

static char keytable[NUM_KEYS][30] = {"PATH",
                                      "INPUT-CLASS",
                                      "INPUT-SIZE",
                                      "RANK",
                                      "DIMENSION-SIZES",
                                      "OUTPUT-CLASS",
                                      "OUTPUT-SIZE",
                                      "OUTPUT-ARCHITECTURE",
                                      "OUTPUT-BYTE-ORDER",
                                      "CHUNKED-DIMENSION-SIZES",
                                      "COMPRESSION-TYPE",
                                      "COMPRESSION-PARAM",
                                      "EXTERNAL-STORAGE",
                                      "MAXIMUM-DIMENSIONS",
                                      "INPUT-BYTE-ORDER"};

static int state_table[15][8] = {
    /* token ordering: FILNAME      OPT_o   OPT_c  OPT_h  OPT_d  OPT_p  OPT_t  OPT_s   */

    /* state 0: start */
    {1, INVALID_TOKEN, INVALID_TOKEN, 6, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN},

    /* state 1: input files */
    {INVALID_TOKEN, INVALID_TOKEN, 2, INVALID_TOKEN, 7, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN},

    /* state 2: -c[onfigfile] */
    {3, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 3: configfile */
    {1, 4, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN},

    /* state 4: -o[utfile] */
    {5, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 5: outfile */
    {INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 6: -h[elp] */
    {INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 7: -d[ims] */
    {8, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 8: dimensions */
    {1, 4, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, 9, 11, 13},

    /* state 9: -p[ath] */
    {10, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 10: path name */
    {1, 4, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, 11, 13},

    /* state 11: -t[ype] */
    {12, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 12: data type */
    {1, 4, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, 13},

    /* state 13: -s[ize] */
    {14, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN,
     INVALID_TOKEN},

    /* state 14: data size */
    {1, 4, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN, INVALID_TOKEN}

};

/*
 *
 *  Function declarations for h5import
 *
 */
void usage(char *);
void setDefaultValues(struct Input *in, int count);
void help(char *);

hid_t createOutputDataType(struct Input *in);
hid_t createInputDataType(struct Input *in);

#endif /* H5IMPORT_H */
