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

/*-------------------------------------------------------------------------
 *
 * Created:         H5Tdbg.c
 *                  Jul 19 2007
 *                  Quincey Koziol
 *
 * Purpose:         Dump debugging information about a datatype
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5T_PACKAGE /*suppress error about including H5Tpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions                        */
#include "H5Eprivate.h" /* Error handling                           */
#include "H5Tpkg.h"     /* Datatypes                                */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5T__print_stats
 *
 * Purpose:     Print statistics about a conversion path.  Statistics are
 *              printed only if all the following conditions are true:
 *
 *              1. The library was compiled with H5T_DEBUG defined.
 *              2. Data type debugging is turned on at run time.
 *              3. The path was called at least one time.
 *
 *              The optional NPRINT argument keeps track of the number of
 *              conversions paths for which statistics have been shown. If
 *              its value is zero then table headers are printed before the
 *              first line of output.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__print_stats(H5T_path_t H5_ATTR_UNUSED *path, int H5_ATTR_UNUSED *nprint /*in,out*/)
{
#ifdef H5T_DEBUG
    hsize_t nbytes;
    char    bandwidth[32];
#endif

    FUNC_ENTER_PACKAGE_NOERR

#ifdef H5T_DEBUG
    if (H5DEBUG(T) && path->stats.ncalls > 0) {
        if (nprint && 0 == (*nprint)++) {
            HDfprintf(H5DEBUG(T), "H5T: type conversion statistics:\n");
            HDfprintf(H5DEBUG(T), "   %-16s %10s %10s %8s %8s %8s %10s\n", "Conversion", "Elmts", "Calls",
                      "User", "System", "Elapsed", "Bandwidth");
            HDfprintf(H5DEBUG(T), "   %-16s %10s %10s %8s %8s %8s %10s\n", "----------", "-----", "-----",
                      "----", "------", "-------", "---------");
        } /* end if */

        if (path->src && path->dst)
            nbytes = MAX(H5T_get_size(path->src), H5T_get_size(path->dst));
        else if (path->src)
            nbytes = H5T_get_size(path->src);
        else if (path->dst)
            nbytes = H5T_get_size(path->dst);
        else
            nbytes = 0;

        nbytes *= path->stats.nelmts;
        H5_bandwidth(bandwidth, (double)nbytes, path->stats.timer.etime);
        HDfprintf(H5DEBUG(T), "   %-16s %10" PRIdHSIZE " %10d %8.2f %8.2f %8.2f %10s\n", path->name,
                  path->stats.nelmts, path->stats.ncalls, path->stats.timer.utime, path->stats.timer.stime,
                  path->stats.timer.etime, bandwidth);
    }
#endif

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5T__print_stats() */

/*-------------------------------------------------------------------------
 * Function:    H5T_debug
 *
 * Purpose:     Prints information about a data type.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_debug(const H5T_t *dt, FILE *stream)
{
    const char *s1 = "";
    const char *s2 = "";
    unsigned    i;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(dt);
    HDassert(stream);

    switch (dt->shared->type) {
        case H5T_INTEGER:
            s1 = "int";
            break;

        case H5T_FLOAT:
            s1 = "float";
            break;

        case H5T_TIME:
            s1 = "time";
            break;

        case H5T_STRING:
            s1 = "str";
            break;

        case H5T_BITFIELD:
            s1 = "bits";
            break;

        case H5T_OPAQUE:
            s1 = "opaque";
            break;

        case H5T_COMPOUND:
            s1 = "struct";
            break;

        case H5T_ENUM:
            s1 = "enum";
            break;

        case H5T_VLEN:
            if (H5T_IS_VL_STRING(dt->shared))
                s1 = "str";
            else
                s1 = "vlen";
            break;

        default:
            s1 = "";
            break;
    } /* end switch */

    switch (dt->shared->state) {
        case H5T_STATE_TRANSIENT:
            s2 = "[transient]";
            break;

        case H5T_STATE_RDONLY:
            s2 = "[constant]";
            break;

        case H5T_STATE_IMMUTABLE:
            s2 = "[predefined]";
            break;

        case H5T_STATE_NAMED:
            s2 = "[named,closed]";
            break;

        case H5T_STATE_OPEN:
            s2 = "[named,open]";
            break;
        default:
            HDassert(0 && "This Should never be executed!");
    } /* end switch */

    HDfprintf(stream, "%s%s {nbytes=%lu", s1, s2, (unsigned long)(dt->shared->size));

    if (H5T_IS_ATOMIC(dt->shared)) {
        uint64_t tmp;

        switch (dt->shared->u.atomic.order) {
            case H5T_ORDER_BE:
                s1 = "BE";
                break;

            case H5T_ORDER_LE:
                s1 = "LE";
                break;

            case H5T_ORDER_VAX:
                s1 = "VAX";
                break;

            case H5T_ORDER_NONE:
                s1 = "NONE";
                break;

            default:
                s1 = "order?";
                break;
        } /* end switch */

        HDfprintf(stream, ", %s", s1);

        if (dt->shared->u.atomic.offset)
            HDfprintf(stream, ", offset=%lu", (unsigned long)(dt->shared->u.atomic.offset));
        if (dt->shared->u.atomic.prec != 8 * dt->shared->size)
            HDfprintf(stream, ", prec=%lu", (unsigned long)(dt->shared->u.atomic.prec));

        switch (dt->shared->type) {
            case H5T_INTEGER:
                switch (dt->shared->u.atomic.u.i.sign) {
                    case H5T_SGN_NONE:
                        s1 = "unsigned";
                        break;

                    case H5T_SGN_2:
                        s1 = NULL;
                        break;

                    default:
                        s1 = "sign?";
                        break;

                } /* end switch */
                if (s1)
                    HDfprintf(stream, ", %s", s1);
                break;

            case H5T_FLOAT:
                switch (dt->shared->u.atomic.u.f.norm) {
                    case H5T_NORM_IMPLIED:
                        s1 = "implied";
                        break;

                    case H5T_NORM_MSBSET:
                        s1 = "msbset";
                        break;

                    case H5T_NORM_NONE:
                        s1 = "no-norm";
                        break;

                    default:
                        s1 = "norm?";
                        break;
                } /* end switch */

                HDfprintf(stream, ", sign=%lu+1", (unsigned long)(dt->shared->u.atomic.u.f.sign));
                HDfprintf(stream, ", mant=%lu+%lu (%s)", (unsigned long)(dt->shared->u.atomic.u.f.mpos),
                          (unsigned long)(dt->shared->u.atomic.u.f.msize), s1);
                HDfprintf(stream, ", exp=%lu+%lu", (unsigned long)(dt->shared->u.atomic.u.f.epos),
                          (unsigned long)(dt->shared->u.atomic.u.f.esize));
                tmp = dt->shared->u.atomic.u.f.ebias >> 32;
                if (tmp) {
                    size_t hi = (size_t)tmp;
                    size_t lo = (size_t)(dt->shared->u.atomic.u.f.ebias & 0xffffffff);
                    HDfprintf(stream, " bias=0x%08lx%08lx", (unsigned long)hi, (unsigned long)lo);
                }
                else {
                    size_t lo = (size_t)(dt->shared->u.atomic.u.f.ebias & 0xffffffff);
                    HDfprintf(stream, " bias=0x%08lx", (unsigned long)lo);
                }
                break;

            default:
                /* No additional info */
                break;
        } /* end switch */
    }
    else if (H5T_COMPOUND == dt->shared->type) {
        /* Compound data type */
        for (i = 0; i < dt->shared->u.compnd.nmembs; i++) {
            HDfprintf(stream, "\n\"%s\" @%lu", dt->shared->u.compnd.memb[i].name,
                      (unsigned long)(dt->shared->u.compnd.memb[i].offset));
            HDfprintf(stream, " ");
            H5T_debug(dt->shared->u.compnd.memb[i].type, stream);
        } /* end for */
        HDfprintf(stream, "\n");
    }
    else if (H5T_VLEN == dt->shared->type) {
        switch (dt->shared->u.vlen.loc) {
            case H5T_LOC_MEMORY:
                HDfprintf(stream, ", loc=memory");
                break;

            case H5T_LOC_DISK:
                HDfprintf(stream, ", loc=disk");
                break;

            default:
                HDfprintf(stream, ", loc=UNKNOWN");
                break;
        } /* end switch */

        if (H5T_IS_VL_STRING(dt->shared))
            /* Variable length string datatype */
            HDfprintf(stream, ", variable-length");
        else {
            /* Variable length sequence datatype */
            HDfprintf(stream, " VLEN ");
            H5T_debug(dt->shared->parent, stream);
            HDfprintf(stream, "\n");
        } /* end else */
    }
    else if (H5T_ENUM == dt->shared->type) {
        size_t base_size;

        /* Enumeration data type */
        HDfprintf(stream, " ");
        H5T_debug(dt->shared->parent, stream);
        base_size = dt->shared->parent->shared->size;
        for (i = 0; i < dt->shared->u.enumer.nmembs; i++) {
            size_t k;

            HDfprintf(stream, "\n\"%s\" = 0x", dt->shared->u.enumer.name[i]);
            for (k = 0; k < base_size; k++)
                HDfprintf(stream, "%02lx", (unsigned long)(dt->shared->u.enumer.value + (i * base_size) + k));
        } /* end for */
        HDfprintf(stream, "\n");
    }
    else if (H5T_OPAQUE == dt->shared->type) {
        HDfprintf(stream, ", tag=\"%s\"", dt->shared->u.opaque.tag);
    }
    else {
        /* Unknown */
        HDfprintf(stream, "unknown class %d\n", (int)(dt->shared->type));
    }
    HDfprintf(stream, "}");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5T_debug() */
