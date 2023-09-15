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
 * Common operations for "remote" processes for the mirror VFD
 */

#include "mirror_remote.h"

#ifdef H5_HAVE_MIRROR_VFD

/* ---------------------------------------------------------------------------
 * Function:    mirror_log
 *
 * Purpose:     Write message to the logging stream/file.
 *              If logging info pointer is NULL, uses logging defaults.
 * ----------------------------------------------------------------------------
 */
void
mirror_log(struct mirror_log_info *info, unsigned int level, const char *format, ...)
{
    FILE        *stream    = MIRROR_LOG_DEFAULT_STREAM;
    unsigned int verbosity = MIRROR_LOG_DEFAULT_VERBOSITY;
    bool         custom    = false;

    if (info != NULL && info->magic == MIRROR_LOG_INFO_MAGIC) {
        stream    = info->stream;
        verbosity = info->verbosity;
        custom    = true;
    }

    if (level == V_NONE) {
        return;
    }
    else if (level <= verbosity) {
        if (custom == true && info->prefix[0] != '\0') {
            fprintf(stream, "%s", info->prefix);
        }

        switch (level) {
            case (V_ERR):
                fprintf(stream, "ERROR ");
                break;
            case (V_WARN):
                fprintf(stream, "WARNING ");
                break;
            default:
                break;
        }

        if (format != NULL) {
            va_list args;
            va_start(args, format);
            vfprintf(stream, format, args);
            va_end(args);
        }

        fprintf(stream, "\n");
        fflush(stream);
    } /* end if sufficiently verbose to print */
} /* end mirror_log() */

/* ---------------------------------------------------------------------------
 * Function:    session_log_bytes
 *
 * Purpose:     "Pretty-print" raw binary data to logging stream/file.
 *              If info pointer is NULL, uses logging defaults.
 * ----------------------------------------------------------------------------
 */
void
mirror_log_bytes(struct mirror_log_info *info, unsigned int level, size_t n_bytes, const unsigned char *buf)
{
    FILE        *stream    = MIRROR_LOG_DEFAULT_STREAM;
    unsigned int verbosity = MIRROR_LOG_DEFAULT_VERBOSITY;

    if (buf == NULL) {
        return;
    }

    if (info != NULL && info->magic == MIRROR_LOG_INFO_MAGIC) {
        stream    = info->stream;
        verbosity = info->verbosity;
    }

    if (level <= verbosity) {
        size_t               bytes_written = 0;
        const unsigned char *b             = NULL;

        /* print whole lines */
        while ((n_bytes - bytes_written) >= 32) {
            b = buf + bytes_written; /* point to region in buffer */
            fprintf(stream,
                    "%04zX  %02X%02X%02X%02X %02X%02X%02X%02X"
                    " %02X%02X%02X%02X %02X%02X%02X%02X"
                    " %02X%02X%02X%02X %02X%02X%02X%02X"
                    " %02X%02X%02X%02X %02X%02X%02X%02X\n",
                    bytes_written, b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8], b[9], b[10], b[11],
                    b[12], b[13], b[14], b[15], b[16], b[17], b[18], b[19], b[20], b[21], b[22], b[23], b[24],
                    b[25], b[26], b[27], b[28], b[29], b[30], b[31]);
            bytes_written += 32;
        }

        /* start partial line */
        if (n_bytes > bytes_written) {
            fprintf(stream, "%04zX ", bytes_written);
        }

        /* partial line blocks */
        while ((n_bytes - bytes_written) >= 4) {
            fprintf(stream, " %02X%02X%02X%02X", buf[bytes_written], buf[bytes_written + 1],
                    buf[bytes_written + 2], buf[bytes_written + 3]);
            bytes_written += 4;
        }

        /* block separator before partial block */
        if (n_bytes > bytes_written) {
            fprintf(stream, " ");
        }

        /* partial block individual bytes */
        while (n_bytes > bytes_written) {
            fprintf(stream, "%02X", buf[bytes_written++]);
        }

        /* end partial line */
        fprintf(stream, "\n");
    } /* end if suitably verbose to log */
} /* end mirror_log_bytes() */

/* ---------------------------------------------------------------------------
 * Function:    mirror_log_init
 *
 * Purpose:     Prepare a loginfo_t structure for use.
 *
 * Return:      Success: Pointer to newly-ceated info.
 *              Failure: NULL. Either unable to allocate or cannot open file.
 * ----------------------------------------------------------------------------
 */
loginfo_t *
mirror_log_init(char *path, const char *prefix, unsigned int verbosity)
{
    loginfo_t *info = NULL;

    info = (loginfo_t *)malloc(sizeof(loginfo_t));
    if (info != NULL) {
        info->magic     = MIRROR_LOG_INFO_MAGIC;
        info->verbosity = verbosity;
        info->stream    = MIRROR_LOG_DEFAULT_STREAM;
        info->prefix[0] = '\0';

        if (prefix && *prefix) {
            strncpy(info->prefix, prefix, MIRROR_LOG_PREFIX_MAX);
        }

        if (path && *path) {
            FILE *f = NULL;
            f       = fopen(path, "w");
            if (NULL == f) {
                fprintf(MIRROR_LOG_DEFAULT_STREAM, "WARN custom logging path could not be opened: %s\n",
                        path);
                info->magic += 1;
                free(info);
            }
            else {
                info->stream = f;
            }
        }

    } /* end if able to allocate */

    return info;
} /* end mirror_log_init() */

/* ---------------------------------------------------------------------------
 * Function:    mirror_log_term
 *
 * Purpose:     Shut down and clean up a loginfo_t structure.
 *
 * Return:      Success: SUCCEED. Resources released.
 *              Failure: FAIL.    Indeterminite state.
 * ----------------------------------------------------------------------------
 */
herr_t
mirror_log_term(loginfo_t *info)
{
    if (info == NULL || info->magic != MIRROR_LOG_INFO_MAGIC) {
        return FAIL;
    }
    if (info->stream != stderr || info->stream != stdout) {
        if (fclose(info->stream) < 0) {
            return FAIL;
        }
    }
    info->magic += 1;
    free(info);
    return SUCCEED;
} /* end mirror_log_term() */

#endif /* H5_HAVE_MIRROR_VFD */
