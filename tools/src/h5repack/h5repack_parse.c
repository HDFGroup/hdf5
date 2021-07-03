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

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
 * Function: parse_filter
 *
 * Purpose: read filter information
 *
 * Return: a list of names, the number of names and its compression type
 *
 * <name of filter> can be:
 *  GZIP, to apply the HDF5 GZIP filter (GZIP compression)
 *  SZIP, to apply the HDF5 SZIP filter (SZIP compression)
 *  SHUF, to apply the HDF5 shuffle filter
 *  FLET, to apply the HDF5 checksum filter
 *  NBIT, to apply the HDF5 NBIT filter (NBIT compression)
 *  SOFF, to apply the HDF5 scale+offset filter (compression)
 *  UD, to apply a User Defined filter k,m,n1[,…,nm]
 *  NONE, to remove the filter
 *
 * Examples:
 * "GZIP=6"
 * "A,B:NONE"
 *-------------------------------------------------------------------------
 */
obj_list_t *
parse_filter(const char *str, unsigned *n_objs, filter_info_t *filt, pack_opt_t *options, int *is_glb)
{
    size_t      i, m, u;
    char        c;
    size_t      len = HDstrlen(str);
    int         f, k, l, p, q, end_obj = -1, no_param = 0;
    unsigned    j, n;
    char        sobj[MAX_NC_NAME];
    char        scomp[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    char        stype[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    char        smask[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    obj_list_t *obj_list  = NULL;
    unsigned    pixels_per_block;

    /* initialize compression  info */
    HDmemset(filt, 0, sizeof(filter_info_t));
    *is_glb = 0;

    /* check for the end of object list and number of objects */
    for (i = 0, n = 0; i < len; i++) {
        c = str[i];
        if (c == ':') {
            end_obj = (int)i;
            break;
        }
        if (c == ',')
            n++;
    }
    n++;

    /* Check for missing : */
    if (end_obj == -1) {
        /* apply to all objects */
        options->all_filter = 1;
        *is_glb             = 1;
        *n_objs             = 1;
    }
    else
        *n_objs = n;

    obj_list = (obj_list_t *)HDmalloc(n * sizeof(obj_list_t));
    if (obj_list == NULL) {
        error_msg("could not allocate object list\n");
        return NULL;
    }

    /* get object list */
    if (end_obj > 0)
        for (j = 0, k = 0, n = 0; j < (unsigned)end_obj; j++, k++) {
            c       = str[j];
            sobj[k] = c;
            if (c == ',' || j == (unsigned)(end_obj - 1)) {
                if (c == ',')
                    sobj[k] = '\0';
                else
                    sobj[k + 1] = '\0';

                HDstrcpy(obj_list[n].obj, sobj);
                HDmemset(sobj, 0, sizeof(sobj));
                n++;
                k = -1;
            }
        }
    /* nothing after : */
    if (end_obj + 1 == (int)len) {
        if (obj_list)
            HDfree(obj_list);
        error_msg("input Error: Invalid compression type in <%s>\n", str);
        HDexit(EXIT_FAILURE);
    }

    /* get filter additional parameters */
    m = 0;
    for (i = (size_t)(end_obj + 1), k = 0, j = 0; i < len; i++, k++) {
        c        = str[i];
        scomp[k] = c;
        if (c == '=' || i == len - 1) {
            if (c == '=') {      /*one more parameter */
                scomp[k] = '\0'; /*cut space */
                /*-------------------------------------------------------------------------
                 * H5Z_FILTER_SZIP
                 * szip has the format SZIP=<pixels per block,coding>
                 * pixels per block is a even number in 2-32 and coding method is 'EC' or 'NN'
                 * example SZIP=8,NN
                 *-------------------------------------------------------------------------
                 */
                if (HDstrcmp(scomp, "SZIP") == 0) {
                    l = -1; /* mask index check */
                    for (m = 0, u = i + 1; u < len; u++, m++) {
                        if (str[u] == ',') {
                            stype[m] = '\0'; /* end digit of szip */
                            l        = 0;    /* start EC or NN search */
                            u++;             /* skip ',' */
                        }
                        c = str[u];
                        if (!HDisdigit(c) && l == -1) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("compression parameter not digit in <%s>\n", str);
                            HDexit(EXIT_FAILURE);
                        }
                        if (l == -1)
                            stype[m] = c;
                        else {
                            smask[l] = c;
                            l++;
                            if (l == 2) {
                                smask[l] = '\0';
                                i        = len - 1; /* end */
                                if (HDstrcmp(smask, "NN") == 0)
                                    filt->cd_values[j++] = H5_SZIP_NN_OPTION_MASK;
                                else if (HDstrcmp(smask, "EC") == 0)
                                    filt->cd_values[j++] = H5_SZIP_EC_OPTION_MASK;
                                else {
                                    error_msg("szip mask must be 'NN' or 'EC' \n");
                                    HDexit(EXIT_FAILURE);
                                }
                            }
                        }
                    } /* u */
                }     /*if */

                /*-------------------------------------------------------------------------
                 * H5Z_FILTER_SCALEOFFSET
                 * scaleoffset has the format SOFF=<scale_factor,scale_type>
                 * scale_type can be
                 *   integer datatype, H5Z_SO_INT (IN)
                 *   float datatype using D-scaling method, H5Z_SO_FLOAT_DSCALE  (DS)
                 *   float datatype using E-scaling method, H5Z_SO_FLOAT_ESCALE  (ES) , not yet implemented
                 * for integer datatypes, scale_factor denotes Minimum Bits
                 * for float datatypes, scale_factor denotes decimal scale factor
                 *  examples
                 *  SOFF=31,IN
                 *  SOFF=3,DF
                 *-------------------------------------------------------------------------
                 */
                else if (HDstrcmp(scomp, "SOFF") == 0) {
                    l = -1; /* mask index check */
                    for (m = 0, u = i + 1; u < len; u++, m++) {
                        if (str[u] == ',') {
                            stype[m] = '\0'; /* end digit */
                            l        = 0;    /* start 'IN' , 'DS', or 'ES' search */
                            u++;             /* skip ',' */
                        }
                        c = str[u];
                        if (!HDisdigit(c) && l == -1) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("compression parameter is not a digit in <%s>\n", str);
                            HDexit(EXIT_FAILURE);
                        }
                        if (l == -1)
                            stype[m] = c;
                        else {
                            smask[l] = c;
                            l++;
                            if (l == 2) {
                                smask[l] = '\0';
                                i        = len - 1; /* end */
                                if (HDstrcmp(smask, "IN") == 0)
                                    filt->cd_values[j++] = H5Z_SO_INT;
                                else if (HDstrcmp(smask, "DS") == H5Z_SO_FLOAT_DSCALE)
                                    filt->cd_values[j++] = H5Z_SO_FLOAT_DSCALE;
                                else {
                                    error_msg("scale type must be 'IN' or 'DS' \n");
                                    HDexit(EXIT_FAILURE);
                                }
                            }
                        }
                    } /* u */
                }     /*if */

                /*-------------------------------------------------------------------------
                 * User Defined
                 *   has the format
                 *UD=<filter_number,filter_flag,cd_value_count,value_1[,value_2,...,value_N]> BZIP2 example
                 *  UD=307,0,1,9
                 *-------------------------------------------------------------------------
                 */
                else if (HDstrcmp(scomp, "UD") == 0) {
                    l = -1; /* filter number index check */
                    f = -1; /* filter flag index check */
                    p = -1; /* CD_VAL count check */
                    for (m = 0, q = 0, u = i + 1; u < len; u++, m++, q++) {
                        if (str[u] == ',') {
                            stype[q] = '\0'; /* end digit */
                            if (l == -1) {
                                filt->filtn = HDatoi(stype);
                                l           = 0;
                            }
                            else if (f == -1) {
                                filt->filt_flag = (unsigned)HDstrtoul(stype, NULL, 0);
                                f               = 0;
                            }
                            else if (p == -1) {
                                filt->cd_nelmts = HDstrtoull(stype, NULL, 0);
                                p               = 0;
                            }
                            else {
                                filt->cd_values[j++] = (unsigned)HDstrtoul(stype, NULL, 0);
                            }
                            q = 0;
                            u++; /* skip ',' */
                        }
                        c = str[u];
                        if (!HDisdigit(c) && l == -1) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("filter number parameter is not a digit in <%s>\n", str);
                            HDexit(EXIT_FAILURE);
                        }
                        else if (!HDisdigit(c) && f == -1) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("filter flag parameter is not a digit in <%s>\n", str);
                            HDexit(EXIT_FAILURE);
                        }
                        stype[q] = c;
                    } /* for u */
                    stype[q] = '\0';
                } /*if */

                /*-------------------------------------------------------------------------
                 * all other filters
                 *-------------------------------------------------------------------------
                 */
                else {
                    /* here we could have 1 or 2 digits  */
                    for (m = 0, u = i + 1; u < len; u++, m++) {
                        c = str[u];
                        if (!HDisdigit(c)) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("compression parameter is not a digit in <%s>\n", str);
                            HDexit(EXIT_FAILURE);
                        }
                        stype[m] = c;
                    } /* u */

                    stype[m] = '\0';
                } /*if */

                filt->cd_values[j++] = (unsigned)HDstrtoul(stype, NULL, 0);
                if (filt->cd_nelmts == 0)
                    j = 0;
                i += m; /* jump */
            }
            else if (i == len - 1) { /*no more parameters */
                scomp[k + 1] = '\0';
                no_param     = 1;
            }

            /*-------------------------------------------------------------------------
             * translate from string to filter symbol
             *-------------------------------------------------------------------------
             */

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_NONE
             *-------------------------------------------------------------------------
             */
            if (HDstrcmp(scomp, "NONE") == 0) {
                filt->filtn     = H5Z_FILTER_NONE;
                filt->cd_nelmts = 0;
            }

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_DEFLATE
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "GZIP") == 0) {
                filt->filtn     = H5Z_FILTER_DEFLATE;
                filt->cd_nelmts = 1;
                if (no_param) { /*no more parameters, GZIP must have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("missing compression parameter in <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SZIP
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "SZIP") == 0) {
                filt->filtn     = H5Z_FILTER_SZIP;
                filt->cd_nelmts = 2;
                if (no_param) { /*no more parameters, SZIP must have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("missing compression parameter in <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SHUFFLE
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "SHUF") == 0) {
                filt->filtn     = H5Z_FILTER_SHUFFLE;
                filt->cd_nelmts = 0;
                if (m > 0) { /*shuffle does not have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("extra parameter in SHUF <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_FLETCHER32
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "FLET") == 0) {
                filt->filtn     = H5Z_FILTER_FLETCHER32;
                filt->cd_nelmts = 0;
                if (m > 0) { /*shuffle does not have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("extra parameter in FLET <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_NBIT
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "NBIT") == 0) {
                filt->filtn     = H5Z_FILTER_NBIT;
                filt->cd_nelmts = 0;
                if (m > 0) { /*nbit does not have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("extra parameter in NBIT <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SCALEOFFSET
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "SOFF") == 0) {
                filt->filtn     = H5Z_FILTER_SCALEOFFSET;
                filt->cd_nelmts = 2;
                if (no_param) { /*no more parameters, SOFF must have parameter */
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("missing compression parameter in <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }
            /*-------------------------------------------------------------------------
             * User Defined Filter
             *-------------------------------------------------------------------------
             */
            else if (HDstrcmp(scomp, "UD") == 0) {
                /* parameters does not match count */
                if (filt->cd_nelmts != j) {
                    if (obj_list)
                        HDfree(obj_list);
                    error_msg("incorrect number of compression parameters in <%s>\n", str);
                    HDexit(EXIT_FAILURE);
                }
            }
            else {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("invalid filter type in <%s>\n", str);
                HDexit(EXIT_FAILURE);
            }
            break;
        }
    } /*i*/

    /*-------------------------------------------------------------------------
     * check valid parameters
     *-------------------------------------------------------------------------
     */

    switch (filt->filtn) {
        /*-------------------------------------------------------------------------
         * H5Z_FILTER_DEFLATE
         *-------------------------------------------------------------------------
         */
        case H5Z_FILTER_DEFLATE:
            if (filt->cd_values[0] > 9) {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("invalid compression parameter in <%s>\n", str);
                HDexit(EXIT_FAILURE);
            }
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SZIP
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_SZIP:
            pixels_per_block = filt->cd_values[0];
            if ((pixels_per_block % 2) == 1) {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("pixels_per_block is not even in <%s>\n", str);
                HDexit(EXIT_FAILURE);
            }
            if (pixels_per_block > H5_SZIP_MAX_PIXELS_PER_BLOCK) {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("pixels_per_block is too large in <%s>\n", str);
                HDexit(EXIT_FAILURE);
            }
            if ((HDstrcmp(smask, "NN") != 0) && (HDstrcmp(smask, "EC") != 0)) {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("szip mask must be 'NN' or 'EC' \n");
                HDexit(EXIT_FAILURE);
            }
            break;
        default:
            break;
    };

    return obj_list;
}

/*-------------------------------------------------------------------------
 * Function: parse_layout
 *
 * Purpose: read layout info
 *
 * Return: a list of names, the number of names and its chunking info for
 *  chunked. NULL, on error
 * the layout type can be:
 *  CHUNK, to apply chunking layout
 *  CONTI, to apply contiguous layout
 *  COMPA, to apply compact layout
 *
 * Example:
 * "AA,B,CDE:CHUNK=10X10"
 *
 * Programmer: Pedro Vicente
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */
obj_list_t *
parse_layout(const char *str, unsigned *n_objs, pack_info_t *pack, /* info about layout needed */
             pack_opt_t *options)
{
    obj_list_t *obj_list = NULL;
    unsigned    i, j, n;
    char        c;
    size_t      len = HDstrlen(str);
    int         k, end_obj = -1, c_index;
    char        sobj[MAX_NC_NAME];
    char        sdim[10];
    char        slayout[10];

    HDmemset(sdim, '\0', sizeof(sdim));
    HDmemset(sobj, '\0', sizeof(sobj));
    HDmemset(slayout, '\0', sizeof(slayout));

    /* check for the end of object list and number of objects */
    for (i = 0, n = 0; i < len; i++) {
        c = str[i];
        if (c == ':')
            end_obj = (int)i;
        if (c == ',')
            n++;
    }

    if (end_obj == -1) { /* missing : chunk all */
        options->all_layout = 1;
    }

    n++;
    obj_list = (obj_list_t *)HDmalloc(n * sizeof(obj_list_t));
    if (obj_list == NULL) {
        error_msg("could not allocate object list\n");
        return NULL;
    }
    *n_objs = n;

    /* get object list */
    if (end_obj > 0)
        for (j = 0, k = 0, n = 0; j < (unsigned)end_obj; j++, k++) {
            c       = str[j];
            sobj[k] = c;
            if (c == ',' || j == (unsigned)(end_obj - 1)) {
                if (c == ',')
                    sobj[k] = '\0';
                else
                    sobj[k + 1] = '\0';
                HDstrcpy(obj_list[n].obj, sobj);
                HDmemset(sobj, 0, sizeof(sobj));
                n++;
                k = -1;
            }
        }

    /* nothing after : */
    if (end_obj + 1 == (int)len) {
        if (obj_list)
            HDfree(obj_list);
        error_msg("in parse layout, no characters after : in <%s>\n", str);
        HDexit(EXIT_FAILURE);
    }

    /* get layout info */
    for (j = (unsigned)(end_obj + 1), n = 0; n <= 5; j++, n++) {
        if (n == 5) {
            slayout[n] = '\0'; /*cut string */
            if (HDstrcmp(slayout, "COMPA") == 0)
                pack->layout = H5D_COMPACT;
            else if (HDstrcmp(slayout, "CONTI") == 0)
                pack->layout = H5D_CONTIGUOUS;
            else if (HDstrcmp(slayout, "CHUNK") == 0)
                pack->layout = H5D_CHUNKED;
            else {
                error_msg("in parse layout, not a valid layout in <%s>\n", str);
                HDexit(EXIT_FAILURE);
            }
        }
        else {
            c          = str[j];
            slayout[n] = c;
        }
    } /* j */

    if (pack->layout == H5D_CHUNKED) {
        /*-------------------------------------------------------------------------
         * get chunk info
         *-------------------------------------------------------------------------
         */
        k = 0;
        if (j > len) {
            if (obj_list)
                HDfree(obj_list);
            error_msg("in parse layout,  <%s> Chunk dimensions missing\n", str);
            HDexit(EXIT_FAILURE);
        }

        for (i = j, c_index = 0; i < len; i++) {
            c       = str[i];
            sdim[k] = c;
            k++; /*increment sdim index */

            if (!HDisdigit(c) && c != 'x' && c != 'N' && c != 'O' && c != 'N' && c != 'E') {
                if (obj_list)
                    HDfree(obj_list);
                error_msg("in parse layout, <%s> Not a valid character in <%s>\n", sdim, str);
                HDexit(EXIT_FAILURE);
            }

            if (c == 'x' || i == len - 1) {
                if (c == 'x') {
                    sdim[k - 1]                        = '\0';
                    k                                  = 0;
                    pack->chunk.chunk_lengths[c_index] = HDstrtoull(sdim, NULL, 0);
                    if (pack->chunk.chunk_lengths[c_index] == 0) {
                        if (obj_list)
                            HDfree(obj_list);
                        error_msg("in parse layout, <%s> conversion to number in <%s>\n", sdim, str);
                        HDexit(EXIT_FAILURE);
                    }
                    c_index++;
                }
                else if (i == len - 1) { /*no more parameters */
                    sdim[k] = '\0';
                    k       = 0;
                    if (HDstrcmp(sdim, "NONE") == 0) {
                        pack->chunk.rank = -2;
                    }
                    else {
                        pack->chunk.chunk_lengths[c_index] = HDstrtoull(sdim, NULL, 0);
                        if (pack->chunk.chunk_lengths[c_index] == 0) {
                            if (obj_list)
                                HDfree(obj_list);
                            error_msg("in parse layout, <%s> conversion to number in <%s>\n", sdim, str);
                            HDexit(EXIT_FAILURE);
                        }
                        pack->chunk.rank = c_index + 1;
                    }
                } /*if */
            }     /*if c=='x' || i==len-1 */
        }         /*i*/
    }             /*H5D_CHUNKED*/

    return obj_list;
}
