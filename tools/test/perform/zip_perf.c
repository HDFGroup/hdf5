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

/* ===========================================================================
 * Usage:  zip_perf [-d] [-f] [-h] [-1 to -9] [files...]
 *   -d : decompress
 *   -f : compress with Z_FILTERED
 *   -h : compress with Z_HUFFMAN_ONLY
 *   -1 to -9 : compression level
 */

/* our header files */
#include "h5test.h"
#include "h5tools.h"
#include "h5tools_utils.h"

#ifdef H5_HAVE_FILTER_DEFLATE

#include <zlib.h>

#define ONE_KB 1024
#define ONE_MB (ONE_KB * ONE_KB)
#define ONE_GB (ONE_MB * ONE_KB)

#define MICROSECOND 1000000.0

/* report 0.0 in case t is zero too */
#define MB_PER_SEC(bytes, t) ((fabs(t) < 0.0000000001) ? 0.0 : ((((double)(bytes)) / (double)ONE_MB) / (t)))

#ifndef true
#define true 1
#endif /* true */

#ifndef false
#define false (!true)
#endif /* false */

#ifndef S_IRWXU
#define S_IRWXU (_S_IREAD | _S_IWRITE)
#endif

/* internal variables */
static const char *prog             = NULL;
static const char *option_prefix    = NULL;
static char       *filename         = NULL;
static int         compress_percent = 0;
static int         compress_level   = Z_DEFAULT_COMPRESSION;
static int         output, random_test = false;
static int         report_once_flag;
static double      compression_time;

/* internal functions */
static void error(const char *fmt, ...);
static void compress_buffer(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen);

/* commandline options : long and short form */
static const char            *s_opts   = "hB:b:c:p:rs:0123456789";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},
                                          {"compressability", require_arg, 'c'},
                                          {"file-size", require_arg, 's'},
                                          {"max-buffer-size", require_arg, 'B'},
                                          {"min-buffer-size", require_arg, 'b'},
                                          {"prefix", require_arg, 'p'},
                                          {"random-test", no_arg, 'r'},
                                          {NULL, 0, '\0'}};

/*
 * Function:    error
 * Purpose:     Display error message and exit.
 */
static void
error(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    fprintf(stderr, "%s: error: ", prog);
    H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
    vfprintf(stderr, fmt, ap);
    H5_GCC_CLANG_DIAG_ON("format-nonliteral")
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}

/*
 * Function:    cleanup
 * Purpose:     Cleanup the output file.
 * Returns:     Nothing
 */
static void
cleanup(void)
{
    if (!getenv(HDF5_NOCLEANUP))
        HDunlink(filename);
    free(filename);
}

static void
write_file(Bytef *source, uLongf sourceLen)
{
    Bytef         *d_ptr, *dest;
    uLongf         d_len, destLen;
    struct timeval timer_start, timer_stop;

    /* destination buffer needs to be at least 0.1% larger than sourceLen
     * plus 12 bytes */
    destLen = (uLongf)((double)sourceLen + ((double)sourceLen * 0.1)) + 12;
    dest    = (Bytef *)malloc(destLen);

    if (!dest)
        error("out of memory");

    HDgettimeofday(&timer_start, NULL);
    compress_buffer(dest, &destLen, source, sourceLen);
    HDgettimeofday(&timer_stop, NULL);

    compression_time += ((double)timer_stop.tv_sec + ((double)timer_stop.tv_usec) / MICROSECOND) -
                        ((double)timer_start.tv_sec + ((double)timer_start.tv_usec) / MICROSECOND);

    if (report_once_flag) {
        fprintf(stdout, "\tCompression Ratio: %g\n", ((double)destLen) / (double)sourceLen);
        report_once_flag = 0;
    }

    d_ptr = dest;
    d_len = destLen;

    /* loop to make sure we write everything out that we want to write */
    for (;;) {
        int rc = (int)HDwrite(output, d_ptr, (size_t)d_len);

        if (rc == -1)
            error(strerror(errno));

        if (rc == (int)d_len)
            break;

        d_len -= (size_t)rc;
        d_ptr += rc;
    }

    free(dest);
}

/*
 * Function:    compress_buffer
 * Purpose:     Compress the buffer.
 * Returns:     Z_OK            - success
 *              Z_MEM_ERROR     - not enough memory
 *              Z_BUF_ERROR     - not enough room in the output buffer
 *              Z_STREAM_ERROR  - level parameter is invalid
 */
static void
compress_buffer(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen)
{
    int rc = compress2(dest, destLen, source, sourceLen, compress_level);

    if (rc != Z_OK) {
        /* compress2 failed - cleanup and tell why */
        cleanup();

        switch (rc) {
            case Z_MEM_ERROR:
                error("not enough memory");
                break;
            case Z_BUF_ERROR:
                error("not enough room in the output buffer");
                break;
            case Z_STREAM_ERROR:
                error("level parameter (%d) is invalid", compress_level);
                break;
            default:
                error("unknown compression error");
                break;
        }
    }
}

/*
 * Function:    get_unique_name
 * Purpose:     Create a new file who's name doesn't conflict with
 *              pre-existing files.
 * Returns:     Nothing
 */
#define ZIP_PERF_FILE "zip_perf.data"
static void
get_unique_name(void)
{
    const char *prefix = NULL;
    const char *env    = getenv("HDF5_PREFIX");

    if (env)
        prefix = env;

    if (option_prefix)
        prefix = option_prefix;

    if (prefix)
        /* 2 = 1 for '/' + 1 for null terminator */
        filename = (char *)malloc(strlen(prefix) + strlen(ZIP_PERF_FILE) + 2);
    else
        filename = (char *)malloc(strlen(ZIP_PERF_FILE) + 1);

    if (!filename)
        error("out of memory");

    filename[0] = 0;
    if (prefix) {
        strcpy(filename, prefix);
        strcat(filename, "/");
    }
    strcat(filename, ZIP_PERF_FILE);
}

/*
 * Function:    usage
 * Purpose:     Print a usage message and then exit.
 * Return:      Nothing
 */
static void
usage(void)
{
    fprintf(stdout, "usage: %s [OPTIONS]\n", prog);
    fprintf(stdout, "  OPTIONS\n");
    fprintf(stdout, "     -h, --help                 Print this usage message and exit\n");
    fprintf(stdout, "     -1...-9                    Level of compression, from 1 to 9\n");
    fprintf(stdout, "     -c P, --compressability=P  Percentage of compressability of the random\n");
    fprintf(stdout, "                                data you want [default: 0]");
    fprintf(stdout, "     -s S, --file-size=S        Maximum size of uncompressed file [default: 64M]\n");
    fprintf(stdout, "     -B S, --max-buffer_size=S  Maximum size of buffer [default: 1M]\n");
    fprintf(stdout, "     -b S, --min-buffer_size=S  Minimum size of buffer [default: 128K]\n");
    fprintf(stdout, "     -p D, --prefix=D           The directory prefix to place the file\n");
    fprintf(stdout, "     -r, --random-test          Use random data to write to the file\n");
    fprintf(stdout, "                                [default: no]\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  D  - a directory which exists\n");
    fprintf(stdout, "  P  - a number between 0 and 100\n");
    fprintf(stdout, "  S  - is a size specifier, an integer >=0 followed by a size indicator:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "          K - Kilobyte (%d)\n", ONE_KB);
    fprintf(stdout, "          M - Megabyte (%d)\n", ONE_MB);
    fprintf(stdout, "          G - Gigabyte (%d)\n", ONE_GB);
    fprintf(stdout, "\n");
    fprintf(stdout, "      Example: 37M = 37 Megabytes = %d bytes\n", 37 * ONE_MB);
    fprintf(stdout, "\n");
    fflush(stdout);
}

/*
 * Function:    parse_size_directive
 * Purpose:     Parse the size directive passed on the commandline. The size
 *              directive is an integer followed by a size indicator:
 *
 *                  K, k - Kilobyte
 *                  M, m - Megabyte
 *
 * Return:      The size as a size_t because this is related to buffer size.
 *              If an unknown size indicator is used, then the program will
 *              exit with EXIT_FAILURE as the return value.
 */
static unsigned long
parse_size_directive(const char *size)
{
    unsigned long s;
    char         *endptr;

    s = strtoul(size, &endptr, 10);

    if (endptr && *endptr) {
        while (*endptr != '\0' && (*endptr == ' ' || *endptr == '\t'))
            ++endptr;

        switch (*endptr) {
            case 'K':
            case 'k':
                s *= ONE_KB;
                break;
            case 'M':
            case 'm':
                s *= ONE_MB;
                break;
            case 'G':
            case 'g':
                s *= ONE_GB;
                break;
            default:
                error("illegal size specifier '%c'", *endptr);
                break;
        }
    }

    return s;
}

static void
fill_with_random_data(Bytef *src, uLongf src_len)
{
    unsigned  u;
    h5_stat_t stat_buf;

    memset(&stat_buf, 0, sizeof(h5_stat_t));
    if (HDstat("/dev/urandom", &stat_buf) == 0) {
        uLongf len = src_len;
        Bytef *buf = src;
        int    fd  = HDopen("/dev/urandom", O_RDONLY, 0);

        fprintf(stdout, "Using /dev/urandom for random data\n");

        if (fd < 0)
            error(strerror(errno));

        for (;;) {
            ssize_t rc = HDread(fd, buf, src_len);

            if (rc == -1)
                error(strerror(errno));

            if (rc == (ssize_t)len)
                break;

            buf += rc;
            len -= (size_t)rc;
        }
        HDclose(fd);
    }
    else {
        fprintf(stdout, "Using random() for random data\n");

        for (u = 0; u < src_len; ++u)
            src[u] = (Bytef)(0xff & HDrandom());
    }

    if (compress_percent) {
        size_t s = (size_t)((src_len * (uLongf)compress_percent) / 100);

        memset(src, '\0', s);
    }
}

static void
do_write_test(unsigned long file_size, unsigned long min_buf_size, unsigned long max_buf_size)
{
    uLongf         src_len, total_len;
    struct timeval timer_start, timer_stop;
    double         total_time;
    Bytef         *src;

    for (src_len = min_buf_size; src_len <= max_buf_size; src_len <<= 1) {
        unsigned long i, iters;

        iters = file_size / src_len;
        src   = (Bytef *)calloc(1, sizeof(Bytef) * src_len);

        if (!src) {
            cleanup();
            error("out of memory");
        }

        compression_time = 0.0;

        if (random_test)
            fill_with_random_data(src, src_len);

        fprintf(stdout, "Buffer size == ");

        if (src_len >= ONE_KB && (src_len % ONE_KB) == 0) {
            if (src_len >= ONE_MB && (src_len % ONE_MB) == 0) {
                fprintf(stdout, "%ldMB", src_len / ONE_MB);
            }
            else {
                fprintf(stdout, "%ldKB", src_len / ONE_KB);
            }
        }
        else {
            fprintf(stdout, "%ld", src_len);
        }

        fprintf(stdout, "\n");

        /* do uncompressed data write */
        HDgettimeofday(&timer_start, NULL);
        output = HDopen(filename, O_RDWR | O_CREAT, S_IRWXU);

        if (output == -1)
            error(strerror(errno));

        for (i = 0; i <= iters; ++i) {
            Bytef *s_ptr = src;
            uLong  s_len = src_len;

            /* loop to make sure we write everything out that we want to write */
            for (;;) {
                ssize_t rc = HDwrite(output, s_ptr, s_len);

                if (rc == -1)
                    error(strerror(errno));

                if (rc == (ssize_t)s_len)
                    break;

                s_len -= (size_t)rc;
                s_ptr += rc;
            }
        }

        HDclose(output);
        HDgettimeofday(&timer_stop, NULL);

        total_time = ((double)timer_stop.tv_sec + ((double)timer_stop.tv_usec) / (double)MICROSECOND) -
                     ((double)timer_start.tv_sec + ((double)timer_start.tv_usec) / (double)MICROSECOND);

        fprintf(stdout, "\tUncompressed Write Time: %.2fs\n", total_time);
        fprintf(stdout, "\tUncompressed Write Throughput: %.2fMB/s\n", MB_PER_SEC(file_size, total_time));

        HDunlink(filename);

        /* do compressed data write */
        output = HDopen(filename, O_RDWR | O_CREAT, S_IRWXU);

        if (output == -1)
            error(strerror(errno));

        report_once_flag = 1;
        HDgettimeofday(&timer_start, NULL);

        for (total_len = 0; total_len < file_size; total_len += src_len)
            write_file(src, src_len);

        HDclose(output);
        HDgettimeofday(&timer_stop, NULL);

        total_time = ((double)timer_stop.tv_sec + ((double)timer_stop.tv_usec) / (double)MICROSECOND) -
                     ((double)timer_start.tv_sec + ((double)timer_start.tv_usec) / (double)MICROSECOND);

        fprintf(stdout, "\tCompressed Write Time: %.2fs\n", total_time);
        fprintf(stdout, "\tCompressed Write Throughput: %.2fMB/s\n", MB_PER_SEC(file_size, total_time));
        fprintf(stdout, "\tCompression Time: %gs\n", compression_time);

        HDunlink(filename);
        free(src);
    }
}

/*
 * Function:    main
 * Purpose:     Run the program
 * Return:      EXIT_SUCCESS or EXIT_FAILURE
 */
int
main(int argc, char *argv[])
{
    unsigned long min_buf_size = 128 * ONE_KB, max_buf_size = ONE_MB;
    unsigned long file_size = 64 * ONE_MB;
    int           opt;

    prog = argv[0];

    /* Initialize h5tools lib */
    h5tools_init();

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) > 0) {
        switch ((char)opt) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                compress_level = opt - '0';
                break;
            case 'B':
                max_buf_size = parse_size_directive(H5_optarg);
                break;
            case 'b':
                min_buf_size = parse_size_directive(H5_optarg);
                break;
            case 'c':
                compress_percent = (int)strtol(H5_optarg, NULL, 10);

                if (compress_percent < 0)
                    compress_percent = 0;
                else if (compress_percent > 100)
                    compress_percent = 100;

                break;
            case 'p':
                option_prefix = H5_optarg;
                break;
            case 'r':
                random_test = true;
                break;
            case 's':
                file_size = parse_size_directive(H5_optarg);
                break;
            case '?':
                usage();
                exit(EXIT_FAILURE);
                break;
            case 'h':
            default:
                usage();
                exit(EXIT_SUCCESS);
                break;
        }
    }

    if (min_buf_size > max_buf_size)
        error("minimum buffer size (%d) exceeds maximum buffer size (%d)", min_buf_size, max_buf_size);

    fprintf(stdout, "Filesize: %ld\n", file_size);

    if (compress_level == Z_DEFAULT_COMPRESSION)
        fprintf(stdout, "Compression Level: 6\n");
    else
        fprintf(stdout, "Compression Level: %d\n", compress_level);

    get_unique_name();
    do_write_test(file_size, min_buf_size, max_buf_size);
    cleanup();
    return EXIT_SUCCESS;
}

#else

/*
 * Function:    main
 * Purpose:     Dummy main() function for if HDF5 was configured without
 *              zlib stuff.
 * Return:      EXIT_SUCCESS
 */
int
main(void)
{
    fprintf(stdout, "No compression IO performance because zlib was not configured\n");
    return EXIT_SUCCESS;
}

#endif /* !H5_HAVE_FILTER_DEFLATE */
