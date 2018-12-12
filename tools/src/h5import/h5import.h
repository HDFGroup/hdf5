/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *
 * Data and structure definitions for h5import
 *
 */

#ifndef H5IMPORT_H__
#define H5IMPORT_H__

/*
 * state table tokens
 */
#define FILNAME 0
/* filename */
#define OPT_o   1
/* output filename */
#define OPT_c   2   /* configuration filename */
#define OPT_h   3   /* request for explanation */
#define OPT_d   4   /* dimensions */
#define OPT_p   5   /* pathname */
#define OPT_t   6   /* data type */
#define OPT_s   7   /* data size */
#define ERR    20  /* invalid token */

#define MAX_GROUPS_IN_PATH  20
#define MAX_PATH_NAME_LENGTH 255
#define NUM_KEYS 15
#define MIN_NUM_DIMENSION  1
#define MAX_NUM_DIMENSION  32
#define BASE_10 10

#define PATH             0
#define INPUT_CLASS      1
#define INPUT_SIZE       2
#define RANK             3
#define DIM              4
#define OUTPUT_CLASS     5
#define OUTPUT_SIZE      6
#define OUTPUT_ARCH      7
#define OUTPUT_B_ORDER   8
#define CHUNK            9
#define COMPRESS         10
#define COMPRESS_PARAM   11
#define EXTERNALSTORE    12
#define EXTEND           13
#define INPUT_B_ORDER    14

/* data types */
#define H5DT_INT8      signed char
#define H5DT_INT16     short
#define H5DT_INT32     int
#define H5DT_FLOAT32   float
#define H5DT_FLOAT64   double
#define VOIDP          void*
#define H5DT_UINT8     unsigned char
#define H5DT_UINT16    unsigned short
#define H5DT_UINT32    unsigned int
#define H5DT_INT64     long long
#define H5DT_UINT64    unsigned H5DT_INT64

struct path_info
{
    char group[MAX_GROUPS_IN_PATH][MAX_PATH_NAME_LENGTH];
    int count;
};

struct Input
{
    int h5dumpInput;
    struct path_info path;
    int inputClass;
    int inputSize;
    int inputArchitecture;
    int inputByteOrder;
    int rank;
    hsize_t* sizeOfDimension;
    int outputClass;
    int outputSize;
    int outputArchitecture;
    int outputByteOrder;
    hsize_t* sizeOfChunk;
    hsize_t* maxsizeOfDimension;
    int compressionType;
    int compressionParam;
    char *externFilename;
    VOIDP data;
    int configOptionVector[NUM_KEYS];
};

struct infilesformat
{
    char datafile[MAX_PATH_NAME_LENGTH];
    char configfile[MAX_PATH_NAME_LENGTH];
    struct Input in;
    int config; /* Configfile present? No - 0. Yes - 1 */
};

struct Options
{
    struct infilesformat  infiles[30];  /* structure to hold the list of input file names. Limited to 30*/
    char   outfile[256];  /* output file name */
    int    fcount;       /* number of input files */
};

char keytable[NUM_KEYS][30] = {
        "PATH",
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
        "INPUT-BYTE-ORDER"
};

static int  state_table[15][8] =
{
    /* token ordering: FILNAME      OPT_o   OPT_c  OPT_h  OPT_d  OPT_p  OPT_t  OPT_s   */

    /* state 0: start */
    {1, ERR, ERR, 6, ERR, ERR, ERR, ERR},

    /* state 1: input files */
    {ERR, ERR, 2, ERR, 7, ERR, ERR, ERR},

    /* state 2: -c[onfigfile] */
    {3, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 3: configfile */
    {1, 4, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 4: -o[utfile] */
    {5, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 5: outfile */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 6: -h[elp] */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 7: -d[ims] */
    {8, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 8: dimensions */
    {1, 4, ERR, ERR, ERR, 9, 11, 13},

    /* state 9: -p[ath] */
    {10, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 10: path name */
    {1, 4, ERR, ERR, ERR, ERR, 11, 13},

    /* state 11: -t[ype] */
    {12, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 12: data type */
    {1, 4, ERR, ERR, ERR, ERR, ERR, 13},

    /* state 13: -s[ize] */
    {14, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

    /* state 14: data size */
    {1, 4, ERR, ERR, ERR, ERR, ERR, ERR}

};

/*
 *
 *  Function declarations for h5import
 *
 */
void  usage(char *);
void  setDefaultValues(struct Input *in, int count);
void  help(char *);

hid_t       createOutputDataType(struct Input *in);
hid_t       createInputDataType(struct Input *in);

#endif  /* H5IMPORT_H__ */

