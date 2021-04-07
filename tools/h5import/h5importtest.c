/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"

#ifdef H5_HAVE_WIN32_API
#define OPEN_FLAGS "wb"
#else
#define OPEN_FLAGS "w"
#endif

/*
 * Name:
 *      h5importtest
 *
 * Description:
 *      This program creates files that can be
 *      used to test the h5import program.
 *
 */

int
main(void)
{
    int   nrow = 3, ncol = 4, npln = 5;
    int   i, j, k;
    FILE *sp;
    char  machine_order[3] = {0, 0, 0};

    float row4[3], col4[4], pln4[5];
    float rowo4 = 11.0F, colo4 = 21.0F, plno4 = 51.0F;
    float rowi4 = 1.0F, coli4 = 2.0F, plni4 = 5.0F;

    int b32i3[5][3][4];
    int row4i[3], col4i[4], pln4i[5];
    int rowo4i = 11, colo4i = 21, plno4i = 51;
    int rowi4i = 1, coli4i = 2, plni4i = 5;

#ifdef H5_SIZEOF_LONG_LONG
    long long row4i64[3], col4i64[4], pln4i64[5];
    long long rowo4i64 = (long long)11, colo4i64 = (long long)21, plno4i64 = (long long)51;
    long long rowi4i64 = (long long)1, coli4i64 = (long long)2, plni4i64 = (long long)5;
#endif

    short b16i3[5][3][4];
    short row4i16[3], col4i16[4], pln4i16[5];
    short rowo4i16 = (short)11, colo4i16 = (short)21, plno4i16 = (short)51;
    short rowi4i16 = (short)1, coli4i16 = (short)2, plni4i16 = (short)5;

    char b8i3[5][3][4];
    char row4i8[3], col4i8[4], pln4i8[5];
    char rowo4i8 = (char)11, colo4i8 = (char)21, plno4i8 = (char)51;
    char rowi4i8 = (char)1, coli4i8 = (char)2, plni4i8 = (char)5;

    double b64r3[5][3][4];
    double row8[3], col8[4], pln8[5];
    double rowo8 = 11.0F, colo8 = 21.0F, plno8 = 51.0F;
    double rowi8 = 1.0F, coli8 = 2.0F, plni8 = 5.0F;

    /* Initialize machine endian */
    volatile uint32_t ibyte = 0x01234567;
    /* 0 for big endian, 1 for little endian. */
    if ((*((volatile uint8_t *)(&ibyte))) == 0x67)
        HDstrcpy(machine_order, "LE");
    else
        HDstrcpy(machine_order, "BE");

    /*
     * initialize the row, column, and plane vectors
     *
     * row values start at 11 and increment by 1 => 11, 12, 13
     * column values start at 21 and increment by 2 => 21, 23, 25, 27
     * plane values start at 51 and increment by 5 => 51, 56, 61, 66, 71
     */

    /*
     * build array elements - rank 2
     *
     * element value = sum of row value and col values
     */

    row4[0] = rowo4;
    col4[0] = colo4;
    pln4[0] = plno4;

    row8[0] = rowo8;
    col8[0] = colo8;
    pln8[0] = plno8;

    row4i[0] = rowo4i;
    col4i[0] = colo4i;
    pln4i[0] = plno4i;

#ifdef H5_SIZEOF_LONG_LONG
    row4i64[0] = rowo4i64;
    col4i64[0] = colo4i64;
    pln4i64[0] = plno4i64;
#endif

    row4i16[0] = rowo4i16;
    col4i16[0] = colo4i16;
    pln4i16[0] = plno4i16;

    row4i8[0] = rowo4i8;
    col4i8[0] = colo4i8;
    pln4i8[0] = plno4i8;

    for (i = 1; i < nrow; i++) {
        row4[i]  = row4[i - 1] + rowi4;
        row8[i]  = row8[i - 1] + rowi8;
        row4i[i] = row4i[i - 1] + rowi4i;
#ifdef H5_SIZEOF_LONG_LONG
        row4i64[i] = row4i64[i - 1] + rowi4i64;
#endif
        row4i16[i] = (short)(row4i16[i - 1] + rowi4i16);
        row4i8[i]  = (char)(row4i8[i - 1] + rowi4i8);
    }

    for (j = 1; j < ncol; j++) {
        col4[j]  = col4[j - 1] + coli4;
        col8[j]  = col8[j - 1] + coli8;
        col4i[j] = col4i[j - 1] + coli4i;
#ifdef H5_SIZEOF_LONG_LONG
        col4i64[j] = col4i64[j - 1] + coli4i64;
#endif
        col4i16[j] = (short)(col4i16[j - 1] + coli4i16);
        col4i8[j]  = (char)(col4i8[j - 1] + coli4i8);
    }
    for (k = 1; k < npln; k++) {
        pln4[k]  = pln4[k - 1] + plni4;
        pln8[k]  = pln8[k - 1] + plni8;
        pln4i[k] = pln4i[k - 1] + plni4i;
#ifdef H5_SIZEOF_LONG_LONG
        pln4i64[k] = pln4i64[k - 1] + plni4i64;
#endif
        pln4i16[k] = (short)(pln4i16[k - 1] + plni4i16);
        pln4i8[k]  = (char)(pln4i8[k - 1] + plni4i8);
    }

    /*
     * build array elements - rank 3
     *
     * element value = sum of row value, col, and plane values
     */

    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            for (k = 0; k < npln; k++) {
                b64r3[k][i][j] = row8[i] + col8[j] + pln8[k];
                b32i3[k][i][j] = row4i[i] + col4i[j] + pln4i[k];
                b16i3[k][i][j] = (short)(row4i16[i] + col4i16[j] + pln4i16[k]);
                b8i3[k][i][j]  = (char)(row4i8[i] + col4i8[j] + pln4i8[k]);
            }

#ifndef UNICOS

#ifdef REBUILDTEXTFILES
    /*-------------------------------------------------------------------------
     * TOOLTEST txtin8.txt -c $srcdir/testfiles/txtin8.conf -o txtin8.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("txtin8.txt", "w");
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++)
                (void)HDfprintf(sp, "%10u", b8i3[k][i][j]);
            (void)HDfprintf(sp, "\n");
        }
    }
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST txtin16.txt -c $srcdir/testfiles/txtin16.conf -o txtin16.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("txtin16.txt", "w");
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++)
                (void)HDfprintf(sp, "%10u", b16i3[k][i][j]);
            (void)HDfprintf(sp, "\n");
        }
    }
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST txtin32.txt -c $srcdir/testfiles/textin32.conf -o textin32.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("txtin32.txt", "w");
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++)
                (void)HDfprintf(sp, "%10d", b32i3[k][i][j]);
            (void)HDfprintf(sp, "\n");
        }
    }
    (void)HDfclose(sp);
#endif

    /*-------------------------------------------------------------------------
     * TOOLTEST binin32.bin -c binin32.conf -o binin32.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("binin32.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b32i3[k][i][j], sizeof(int), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binin32.conf", "w");
    (void)HDfprintf(sp, "PATH /int/bin/32-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS IN\n");
    (void)HDfprintf(sp, "INPUT-SIZE    32\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER BE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 5 3 4\n");
    (void)HDfprintf(sp, "CHUNKED-DIMENSION-SIZES 1 2 1\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST binuin32.bin -c binuin32.conf -o binuin32.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("binuin32.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b32i3[k][i][j], sizeof(unsigned int), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binuin32.conf", "w");
    (void)HDfprintf(sp, "PATH /int/buin/32-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS UIN\n");
    (void)HDfprintf(sp, "INPUT-SIZE    32\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER LE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 5 3 4\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST binin16.bin -c binin16.conf -o binin16.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("binin16.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b16i3[k][i][j], sizeof(short), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binin16.conf", "w");
    (void)HDfprintf(sp, "PATH /int/bin/16-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS IN\n");
    (void)HDfprintf(sp, "INPUT-SIZE    16\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER LE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 2 3 4\n");
    (void)HDfprintf(sp, "CHUNKED-DIMENSION-SIZES 2 2 2\n");
    (void)HDfprintf(sp, "MAXIMUM-DIMENSIONS -1 -1 8\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST binuin16.bin -c binuin16.conf -o binuin16.h5
     *-------------------------------------------------------------------------
     */
    sp = HDfopen("binuin16.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b16i3[k][i][j], sizeof(unsigned short), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binuin16.conf", "w");
    (void)HDfprintf(sp, "PATH /int/buin/16-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS UIN\n");
    (void)HDfprintf(sp, "INPUT-SIZE    16\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER BE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 2 3 4\n");
    (void)HDfprintf(sp, "CHUNKED-DIMENSION-SIZES 2 2 2\n");
    (void)HDfprintf(sp, "MAXIMUM-DIMENSIONS -1 -1 8\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST binin8.bin -c binin8.conf  -o binin8.h5
     *-------------------------------------------------------------------------
     */

    sp = HDfopen("binin8.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b8i3[k][i][j], sizeof(char), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binin8.conf", "w");
    (void)HDfprintf(sp, "PATH /int/bin/8-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS IN\n");
    (void)HDfprintf(sp, "INPUT-SIZE    8\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-CLASS IN\n");
    (void)HDfprintf(sp, "OUTPUT-SIZE    16\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER LE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 5 3 4\n");
    (void)HDfprintf(sp, "CHUNKED-DIMENSION-SIZES 2 2 2\n");
    (void)HDfprintf(sp, "MAXIMUM-DIMENSIONS -1 -1 -1\n");
    (void)HDfprintf(sp, "COMPRESSION-PARAM 3\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

#endif /* UNICOS */

    /*-------------------------------------------------------------------------
     * TOOLTEST binfp64.bin -c binfp64.conf -o binfp64.h5
     *-------------------------------------------------------------------------
     */

    /*
     * binary 64-bit file - rank 2 & 3
     */

    sp = HDfopen("binfp64.bin", OPEN_FLAGS);
    for (k = 0; k < npln; k++) {
        for (i = 0; i < nrow; i++) {
            for (j = 0; j < ncol; j++) {
                (void)HDfwrite((char *)&b64r3[k][i][j], sizeof(double), 1, sp);
            }
        }
    }
    (void)HDfclose(sp);

    sp = HDfopen("binfp64.conf", "w");
    (void)HDfprintf(sp, "PATH /fp/bin/64-bit\n");
    (void)HDfprintf(sp, "INPUT-CLASS FP\n");
    (void)HDfprintf(sp, "INPUT-SIZE    64\n");
    (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
    (void)HDfprintf(sp, "RANK 3\n");
    (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE IEEE\n");
    (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER LE\n");
    (void)HDfprintf(sp, "DIMENSION-SIZES 5 3 4\n");
    (void)HDfprintf(sp, "CHUNKED-DIMENSION-SIZES 2 2 2\n");
    (void)HDfprintf(sp, "MAXIMUM-DIMENSIONS -1 6 7\n");
    (void)HDfprintf(sp, "COMPRESSION-PARAM 8\n");
    (void)HDfprintf(sp, "\n");
    (void)HDfclose(sp);

    /*-------------------------------------------------------------------------
     * TOOLTEST binin8w.bin -c binin8w.conf -o binin8w.h5
     *-------------------------------------------------------------------------
     */

    {
        /* test CR+LF (13,10) and EOF (26) in windows */
        char bin8w[4] = {13, 10, 26, 0};

        sp = HDfopen("binin8w.bin", OPEN_FLAGS);
        for (i = 0; i < 4; i++) {
            char c = bin8w[i];
            if (HDfwrite(&c, sizeof(char), 1, sp) != 1)
                HDprintf("error writing file\n");
        }
        HDfclose(sp);

        sp = HDfopen("binin8w.conf", "w");
        (void)HDfprintf(sp, "INPUT-CLASS IN\n");
        (void)HDfprintf(sp, "INPUT-SIZE    8\n");
        (void)HDfprintf(sp, "INPUT-BYTE-ORDER %s\n", machine_order);
        (void)HDfprintf(sp, "RANK 1\n");
        (void)HDfprintf(sp, "OUTPUT-CLASS IN\n");
        (void)HDfprintf(sp, "OUTPUT-SIZE    8\n");
        (void)HDfprintf(sp, "OUTPUT-ARCHITECTURE STD\n");
        (void)HDfprintf(sp, "OUTPUT-BYTE-ORDER LE\n");
        (void)HDfprintf(sp, "DIMENSION-SIZES 4\n");
        (void)HDfprintf(sp, "\n");
        (void)HDfclose(sp);
    }
    return (EXIT_SUCCESS);
}
