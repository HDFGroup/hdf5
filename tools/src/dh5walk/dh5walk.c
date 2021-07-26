#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h> /* asctime / localtime */

#include <stdarg.h> /* variable length args */

#include <pwd.h> /* for getpwent */
#include <grp.h> /* for getgrent */
#include <errno.h>
#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

#include "libcircle.h"
#include "dtcmp.h"
#include "mfu.h"
#include "mfu_flist.h"
#include "mfu_errors.h"
#include "mfu_flist_internal.h"

typedef int64_t hid_t;
#define H5P_DEFAULT (hid_t)0
extern int H5F__is_hdf5(const char *name, hid_t fapl);
extern int H5Fis_accessible(const char *name, hid_t fapl);

#ifdef DAOS_SUPPORT
#include "mfu_daos.h"
#endif

static char *user_cmd = NULL;
static char  mpierrstr[MPI_MAX_ERROR_STRING];
static int   mpierrlen;
static int   sg_mpi_rank;

static void dh5tool_flist_write_text(const char *name, mfu_flist bflist);

// getpwent getgrent to read user and group entries

/* keep stats during walk */
uint64_t total_dirs    = 0;
uint64_t total_files   = 0;
uint64_t total_links   = 0;
uint64_t total_unknown = 0;
uint64_t total_bytes   = 0;
/* global flags which indicate whether we need
 * to capture tool outputs into a file...
 * Related to this is whether the stderr should
 * be logged seperately.
 */
#define BUFT_SIZE 131072
/* FIXME: 'buft_max' should probably be configurable.. */
size_t  buft_max   = 64;
size_t  buft_count = 0;
buf_t **buf_cache  = NULL;

int   log_output_in_single_file = 0;
char *output_log_file           = NULL;

int   log_stdout_in_file = 0;
char *txtlog             = NULL;

int   log_errors_in_file = 0;
char *errlog             = NULL;

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define MAX_DISTRIBUTE_SEPARATORS 128
struct distribute_option {
    int      separator_number;
    uint64_t separators[MAX_DISTRIBUTE_SEPARATORS];
};

static void
save_command(char *argv0)
{
    assert(argv0);
    user_cmd = strdup(argv0);
}

static void
create_default_separators(struct distribute_option *option, mfu_flist *flist, uint64_t *size,
                          size_t *separators, uint64_t *global_max_file_size)
{
    /* get local max file size for Allreduce */
    uint64_t local_max_file_size = 0;
    for (uint64_t i = 0; i < *size; i++) {
        uint64_t file_size = mfu_flist_file_get_size(*flist, i);
        if (file_size > local_max_file_size) {
            local_max_file_size = file_size;
        }
    }

    /* get the max file size across all ranks */
    MPI_Allreduce(&local_max_file_size, global_max_file_size, 1, MPI_UINT64_T, MPI_MAX, MPI_COMM_WORLD);

    /* print and convert max file size to appropriate units */
    double      max_size_tmp;
    const char *max_size_units;
    mfu_format_bytes(*global_max_file_size, &max_size_tmp, &max_size_units);
    printf("Max File Size: %.3lf %s\n", max_size_tmp, max_size_units);

    /* round next_pow_2 to next multiple of 10 */
    uint64_t max_magnitude_bin = (uint64_t)((ceil(log2((double)(*global_max_file_size)) / 10)) * 10);

    /* get bin ranges based on max file size */
    option->separators[0] = 1;

    /* plus one is for zero count bin */
    *separators    = (size_t)(max_magnitude_bin / 10);
    uint64_t power = 10;
    for (int i = 1; power <= max_magnitude_bin; i++) {
        double raised_2       = pow(2, (double)(power));
        option->separators[i] = (uint64_t)raised_2;
        power += 10;
    }
}

static int
print_flist_distribution(int file_histogram, struct distribute_option *option, mfu_flist *pflist, int rank)
{
    /* file list to use */
    mfu_flist flist = *pflist;

    /* get local size for each rank, and max file sizes */
    uint64_t size = mfu_flist_size(flist);
    uint64_t global_max_file_size;

    size_t separators = 0;
    if (file_histogram) {
        /* create default separators */
        create_default_separators(option, &flist, &size, &separators, &global_max_file_size);
    }
    else {
        separators = (size_t)option->separator_number;
    }

    /* allocate a count for each bin, initialize the bin counts to 0
     * it is separator + 1 because the last bin is the last separator
     * to the DISTRIBUTE_MAX */
    uint64_t *dist = (uint64_t *)MFU_MALLOC((separators + 1) * sizeof(uint64_t));

    /* initialize the bin counts to 0 */
    for (size_t i = 0; i <= separators; i++) {
        dist[i] = 0;
    }

    /* for each file, identify appropriate bin and increment its count */
    for (size_t i = 0; i < size; i++) {
        /* get the size of the file */
        uint64_t file_size = mfu_flist_file_get_size(flist, i);

        /* loop through the bins and find the one the file belongs to,
         * set last bin to -1, if a bin is not found while looping through the
         * list of file size separators, then it belongs in the last bin
         * so (last file size - MAX bin) */
        int64_t max_bin_flag = -1;
        for (size_t j = 0; j < separators; j++) {
            if (file_size <= option->separators[j]) {
                /* found the bin set bin index & increment its count */
                dist[j]++;

                /* a file for this bin was found so can't belong to
                 * last bin (so set the flag) & exit the loop */
                max_bin_flag = 1;
                break;
            }
        }

        /* if max_bin_flag is still -1 then the file belongs to the last bin */
        if (max_bin_flag < 0) {
            dist[separators]++;
        }
    }

    /* get the total sum across all of the bins */
    uint64_t *disttotal = (uint64_t *)MFU_MALLOC((separators + 1) * sizeof(uint64_t));
    MPI_Allreduce(dist, disttotal, (int)(separators + 1), MPI_UINT64_T, MPI_SUM, MPI_COMM_WORLD);

    /* Print the file distribution */
    if (rank == 0) {
        /* number of files in a bin */
        uint64_t    number;
        double      size_tmp;
        const char *size_units;
        printf("%-27s %s\n", "Range", "Number");
        for (size_t i = 0; i <= separators; i++) {
            printf("%s", "[ ");
            if (i == 0) {
                printf("%7.3lf %3s", 0.000, "B");
            }
            else {
                mfu_format_bytes((uint64_t)option->separators[i - 1], &size_tmp, &size_units);
                printf("%7.3lf %3s", size_tmp, size_units);
            }

            printf("%s", " - ");

            if (file_histogram) {
                mfu_format_bytes((uint64_t)option->separators[i], &size_tmp, &size_units);
                number = disttotal[i];
                mfu_format_bytes((uint64_t)option->separators[i], &size_tmp, &size_units);
                printf("%7.3lf %3s ) %" PRIu64 "\n", size_tmp, size_units, number);
            }
            else {
                if (i == separators) {
                    number = disttotal[i];
                    printf("%10s ) %" PRIu64 "\n", "MAX", number);
                }
                else {
                    number = disttotal[i];
                    mfu_format_bytes((uint64_t)option->separators[i], &size_tmp, &size_units);
                    printf("%7.3lf %3s ) %" PRIu64 "\n", size_tmp, size_units, number);
                }
            }
        }
    }

    /* free the memory used to hold bin counts */
    mfu_free(&disttotal);
    mfu_free(&dist);

    return 0;
}

/* * Search the right position to insert the separator * If the separator exists already, return failure *
 * Otherwise, locate the right position, and move the array forward to save the separator.
 */
static int
distribute_separator_add(struct distribute_option *option, uint64_t separator)
{
    int low = 0;
    int high;
    int middle;
    int pos;
    int count;

    count = option->separator_number;
    option->separator_number++;
    if (option->separator_number > MAX_DISTRIBUTE_SEPARATORS) {
        printf("Too many separators");
        return -1;
    }

    if (count == 0) {
        option->separators[0] = separator;
        return 0;
    }

    high = count - 1;
    while (low < high) {
        middle = (high - low) / 2 + low;
        if (option->separators[middle] == separator)
            return -1;
        /* In the left half */
        else if (option->separators[middle] < separator)
            low = middle + 1;
        /* In the right half */
        else
            high = middle;
    }
    assert(low == high);
    if (option->separators[low] == separator)
        return -1;

    if (option->separators[low] < separator)
        pos = low + 1;
    else
        pos = low;

    if (pos < count)
        memmove(&option->separators[low + 1], &option->separators[low],
                sizeof(*option->separators) * (uint64_t)(count - pos));

    option->separators[pos] = separator;
    return 0;
}

static int
distribution_parse(struct distribute_option *option, const char *string)
{
    char *             ptr;
    char *             next;
    unsigned long long separator;
    char *             str;
    int                status = 0;

    if (strncmp(string, "size", strlen("size")) != 0) {
        return -1;
    }

    option->separator_number = 0;
    if (strlen(string) == strlen("size")) {
        return 0;
    }

    if (string[strlen("size")] != ':') {
        return -1;
    }

    str = MFU_STRDUP(string);
    /* Parse separators */
    ptr  = str + strlen("size:");
    next = ptr;
    while (ptr && ptr < str + strlen(string)) {
        next = strchr(ptr, ',');
        if (next != NULL) {
            *next = '\0';
            next++;
        }

        if (mfu_abtoull(ptr, &separator) != MFU_SUCCESS) {
            printf("Invalid separator \"%s\"\n", ptr);
            status = -1;
            goto out;
        }

        if (distribute_separator_add(option, separator)) {
            printf("Duplicated separator \"%llu\"\n", separator);
            status = -1;
            goto out;
        }

        ptr = next;
    }

out:
    mfu_free(&str);
    return status;
}

static void
print_usage(void)
{
    printf("\n");
    printf("Usage: %s [options] <path> ...\n", user_cmd);
#ifdef DAOS_SUPPORT
    printf("\n");
    printf("DAOS paths can be specified as:\n");
    printf("       daos://<pool>/<cont>[/<path>] | <UNS path>\n");
#endif
    printf("\n");
    printf("Options:\n");
    printf("  -i, --input <file>      - read list from file\n");
    printf("  -o, --output <file>     - write output summary to the named file.\n");
    printf("  -E, --error  <file>     - write processed errors to file in text format\n");
    printf("  -l, --log_text <dir>    - write individual tool outputs to a file. Logs can be written to an "
           "optional named directory.\n");
    printf("  -T, --tool <executable> - name of the HDF5 tool to invoke\n");
    printf("  -h, --help              - print usage\n");
    printf("\n");
    printf("For more information see https://mpifileutils.readthedocs.io. \n");
    printf("\n");
    fflush(stdout);
    return;
}

/* given an index, return pointer to that file element,
 * NULL if index is not in range */
static elem_t *
list_get_elem(flist_t *flist, uint64_t idx)
{
    /* return pointer to element if index is within range */
    uint64_t max = flist->list_count;
    if (idx < max) {
        elem_t *elem = flist->list_index[idx];
        return elem;
    }
    return NULL;
}

/* print information about a file given the index and rank (used in print_files) */
static void
print_file(mfu_flist flist, uint64_t idx)
{
    /* store types as strings for print_file */
    char type_str_unknown[] = "UNK";
    char type_str_dir[]     = "DIR";
    char type_str_file[]    = "REG";
    char type_str_link[]    = "LNK";

    /* get filename */
    const char *file = mfu_flist_file_get_name(flist, idx);

    if (mfu_flist_have_detail(flist)) {
        /* get mode */
        mode_t      mode      = (mode_t)mfu_flist_file_get_mode(flist, idx);
        uint64_t    acc       = mfu_flist_file_get_atime(flist, idx);
        uint64_t    mod       = mfu_flist_file_get_mtime(flist, idx);
        uint64_t    cre       = mfu_flist_file_get_ctime(flist, idx);
        uint64_t    size      = mfu_flist_file_get_size(flist, idx);
        const char *username  = mfu_flist_file_get_username(flist, idx);
        const char *groupname = mfu_flist_file_get_groupname(flist, idx);

        char   access_s[30];
        char   modify_s[30];
        char   create_s[30];
        time_t access_t  = (time_t)acc;
        time_t modify_t  = (time_t)mod;
        time_t create_t  = (time_t)cre;
        size_t access_rc = strftime(access_s, sizeof(access_s) - 1, "%FT%T", localtime(&access_t));
        size_t modify_rc = strftime(modify_s, sizeof(modify_s) - 1, "%b %e %Y %H:%M", localtime(&modify_t));
        size_t create_rc = strftime(create_s, sizeof(create_s) - 1, "%FT%T", localtime(&create_t));
        if (access_rc == 0 || modify_rc == 0 || create_rc == 0) {
            /* error */
            access_s[0] = '\0';
            modify_s[0] = '\0';
            create_s[0] = '\0';
        }

        char mode_format[11];
        mfu_format_mode(mode, mode_format);

        double      size_tmp;
        const char *size_units;
        mfu_format_bytes(size, &size_tmp, &size_units);

        printf("%s %s %s %7.3f %3s %s %s\n", mode_format, username, groupname, size_tmp, size_units, modify_s,
               file);
    }
    else {
        /* get type */
        mfu_filetype type     = mfu_flist_file_get_type(flist, idx);
        char *       type_str = type_str_unknown;
        if (type == MFU_TYPE_DIR) {
            type_str = type_str_dir;
        }
        else if (type == MFU_TYPE_FILE) {
            type_str = type_str_file;
        }
        else if (type == MFU_TYPE_LINK) {
            type_str = type_str_link;
        }

        printf("Type=%s File=%s\n", type_str, file);
    }
}

/* TODO: move this somewhere or modify existing print_file */
/* print information about a file given the index and rank (used in print_files) */
static size_t
print_file_text(mfu_flist flist, uint64_t idx, char *buffer, size_t bufsize)
{
    size_t numbytes = 0;

    /* store types as strings for print_file */
    char type_str_unknown[] = "UNK";
    char type_str_dir[]     = "DIR";
    char type_str_file[]    = "REG";
    char type_str_link[]    = "LNK";

    /* get filename */
    const char *file = mfu_flist_file_get_name(flist, idx);

    if (mfu_flist_have_detail(flist)) {
        /* get mode */
        mode_t mode = (mode_t)mfu_flist_file_get_mode(flist, idx);

        uint64_t    acc       = mfu_flist_file_get_atime(flist, idx);
        uint64_t    mod       = mfu_flist_file_get_mtime(flist, idx);
        uint64_t    cre       = mfu_flist_file_get_ctime(flist, idx);
        uint64_t    size      = mfu_flist_file_get_size(flist, idx);
        const char *username  = mfu_flist_file_get_username(flist, idx);
        const char *groupname = mfu_flist_file_get_groupname(flist, idx);

        char   access_s[30];
        char   modify_s[30];
        char   create_s[30];
        time_t access_t  = (time_t)acc;
        time_t modify_t  = (time_t)mod;
        time_t create_t  = (time_t)cre;
        size_t access_rc = strftime(access_s, sizeof(access_s) - 1, "%FT%T", localtime(&access_t));
        size_t modify_rc = strftime(modify_s, sizeof(modify_s) - 1, "%b %e %Y %H:%M", localtime(&modify_t));
        size_t create_rc = strftime(create_s, sizeof(create_s) - 1, "%FT%T", localtime(&create_t));
        if (access_rc == 0 || modify_rc == 0 || create_rc == 0) {
            /* error */
            access_s[0] = '\0';
            modify_s[0] = '\0';
            create_s[0] = '\0';
        }

        char mode_format[11];
        mfu_format_mode(mode, mode_format);

        double      size_tmp;
        const char *size_units;
        mfu_format_bytes(size, &size_tmp, &size_units);

        numbytes = (size_t)snprintf(buffer, bufsize, "%s %s %s %7.3f %3s %s %s\n", mode_format, username,
                                    groupname, size_tmp, size_units, modify_s, file);
    }
    else {
        /* get type */
        mfu_filetype type     = mfu_flist_file_get_type(flist, idx);
        char *       type_str = type_str_unknown;
        if (type == MFU_TYPE_DIR) {
            type_str = type_str_dir;
        }
        else if (type == MFU_TYPE_FILE) {
            type_str = type_str_file;
        }
        else if (type == MFU_TYPE_LINK) {
            type_str = type_str_link;
        }

        numbytes = (size_t)snprintf(buffer, bufsize, "Type=%s File=%s\n", type_str, file);
    }

    return numbytes;
}

static size_t
get_local_bufsize(uint64_t *bufsize)
{
    size_t total = 0;
    if (buft_count > 0) {
        buf_t *lastbuf   = buf_cache[buft_count - 1];
        size_t remaining = lastbuf->count;
        total            = (lastbuf->bufsize * buft_count) - remaining;
        *bufsize         = (uint64_t)(lastbuf->bufsize);
    }
    return total;
}

static void
dh5tool_flist_write_text(const char *name, mfu_flist bflist)
{
    /* convert handle to flist_t */
    flist_t *flist = (flist_t *)bflist;

    /* get our rank and size of the communicator */
    int rank, ranks;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ranks);

    /* start timer */
    double start_write = MPI_Wtime();

    /* total list items */
    uint64_t all_count = mfu_flist_global_size(flist);

    /* report the filename we're writing to */
    if (mfu_rank == 0) {
        MFU_LOG(MFU_LOG_INFO, "Writing to output file: %s", name);
    }

    /* compute size of buffer needed to hold all data */
    size_t   bufsize = 0;
    uint64_t idx     = 0;
    char *   ptr     = NULL;

    /* if we block things up into 128MB chunks, how many iterations
     * to write everything? */
    // uint64_t maxwrite = 128 * 1024 * 1024;
    uint64_t maxwrite    = 0;
    size_t   local_total = get_local_bufsize(&maxwrite);
    uint64_t iters       = 0;
    if (local_total > 0)
        (uint64_t) local_total / maxwrite;

    if (iters * maxwrite < (uint64_t)local_total) {
        iters++;
    }

    /* get max iterations across all procs */
    uint64_t all_iters;
    MPI_Allreduce(&iters, &all_iters, 1, MPI_UINT64_T, MPI_MAX, MPI_COMM_WORLD);

    /* use mpi io hints to stripe across OSTs */
    MPI_Info info;
    MPI_Info_create(&info);

    /* change number of ranks to string to pass to MPI_Info */
    char str_buf[12];
    sprintf(str_buf, "%d", ranks);

    /* no. of I/O devices for lustre striping is number of ranks */
    MPI_Info_set(info, "striping_factor", str_buf);

    /* open file */
    MPI_Status  status;
    MPI_File    fh;
    const char *datarep = "native";
    int         amode   = MPI_MODE_WRONLY | MPI_MODE_CREATE;

    int mpirc = MPI_File_open(MPI_COMM_WORLD, (const char *)name, amode, info, &fh);
    if (mpirc != MPI_SUCCESS) {
        MPI_Error_string(mpirc, mpierrstr, &mpierrlen);
        MFU_ABORT(1, "Failed to open file for writing: `%s' rc=%d %s", name, mpirc, mpierrstr);
    }

    /* truncate file to 0 bytes */
    mpirc = MPI_File_set_size(fh, 0);
    if (mpirc != MPI_SUCCESS) {
        MPI_Error_string(mpirc, mpierrstr, &mpierrlen);
        MFU_ABORT(1, "Failed to truncate file: `%s' rc=%d %s", name, mpirc, mpierrstr);
    }

    /* set file view to be sequence of datatypes past header */
    mpirc = MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, datarep, MPI_INFO_NULL);
    if (mpirc != MPI_SUCCESS) {
        MPI_Error_string(mpirc, mpierrstr, &mpierrlen);
        MFU_ABORT(1, "Failed to set view on file: `%s' rc=%d %s", name, mpirc, mpierrstr);
    }

    /* compute byte offset to write our element */
    uint64_t offset = 0;
    uint64_t bytes  = (uint64_t)local_total;
    MPI_Exscan(&bytes, &offset, 1, MPI_UINT64_T, MPI_SUM, MPI_COMM_WORLD);
    MPI_Offset write_offset = (MPI_Offset)offset;

    uint64_t written = 0;
    while (all_iters > 0) {
        /* compute number of bytes left to write */
        uint64_t remaining = (uint64_t)local_total - written;

        /* maybe Incr pointer to our next buffer */
        if (remaining == 0) {
            idx++;
            if (buf_cache[idx]->buf == NULL) {
            }
        }

        /* compute count we'll write in this iteration */
        int write_count = (int)maxwrite;
        if (remaining < maxwrite) {
            write_count = (int)remaining;
        }
        /* Get the buffer to output to the selected file */
        ptr = buf_cache[idx]->buf;

        /* collective write of file data */
        mpirc = MPI_File_write_at_all(fh, write_offset, ptr, write_count, MPI_BYTE, &status);
        if (mpirc != MPI_SUCCESS) {
            MPI_Error_string(mpirc, mpierrstr, &mpierrlen);
            MFU_ABORT(1, "Failed to write to file: `%s' rc=%d %s", name, mpirc, mpierrstr);
        }

        /* update our offset into the file */
        write_offset += (MPI_Offset)write_count;

        /* update number of bytes written so far */
        written += (uint64_t)write_count;

        /* update pointer into our buffer */
        ptr += write_count;

        /* decrement our collective write loop counter */
        all_iters--;
    }

    /* free buffer */
    // mfu_free(&buf);

    /* close file */
    mpirc = MPI_File_close(&fh);
    if (mpirc != MPI_SUCCESS) {
        MPI_Error_string(mpirc, mpierrstr, &mpierrlen);
        MFU_ABORT(1, "Failed to close file: `%s' rc=%d %s", name, mpirc, mpierrstr);
    }

    /* free mpi info */
    MPI_Info_free(&info);

    /* end timer */
    double end_write = MPI_Wtime();

    /* report write count, time, and rate */
    if (mfu_rank == 0) {
        double secs = end_write - start_write;
        double rate = 0.0;
        if (secs > 0.0) {
            rate = ((double)all_count) / secs;
        }
        MFU_LOG(MFU_LOG_INFO, "Wrote %lu files in %.3lf seconds (%.3lf files/sec)", all_count, secs, rate);
    }

    return;
}

static void
filter_hdf_files(mfu_flist *pflist, char *regex_exp, int exclude, int name)
{
    mfu_flist flist    = *pflist;
    mfu_flist eligible = mfu_flist_subset(flist);
    uint64_t  idx      = 0;
    uint64_t  files    = mfu_flist_size(flist);
    while (idx < files) {
        mfu_filetype type = mfu_flist_file_get_type(flist, idx);
        if (type == MFU_TYPE_FILE || type == MFU_TYPE_LINK) {
            const char *file       = mfu_flist_file_get_name(flist, idx);
            int         accessible = H5Fis_accessible(file, H5P_DEFAULT);
            if (accessible)
                mfu_flist_file_copy(flist, idx, eligible);
        }
        idx++;
    }

    mfu_flist_summarize(eligible);

    /* assume we'll use the full list */
    // mfu_flist srclist = flist;
    mfu_flist srclist = eligible;

    /* filter the list if needed */
    mfu_flist filtered_flist = MFU_FLIST_NULL;
    if (regex_exp != NULL) {
        /* filter the list based on regex */
        filtered_flist = mfu_flist_filter_regex(eligible, regex_exp, exclude, name);

        /* update our source list to use the filtered list instead of the original */
        srclist = filtered_flist;
    }

    mfu_flist_free(&flist);
    *pflist = srclist;
    return;
}

static int
count_dirpaths(int argc, int startcnt, char **argv, int **index_out)
{
    int  k;
    int  path_cnt  = 0;
    int  idx_count = (argc - startcnt);
    int *index     = NULL;
    if (idx_count > 0) {
        index = (int *)malloc((size_t)(argc - startcnt) * sizeof(int));
        assert(index);
    }
    else
        return 0;

    for (k = startcnt; k < argc; k++) {
        char *slash = NULL;
        int   c     = *argv[k];
        if ((c == '.') || (c == '/')) {
            index[path_cnt++] = k;
        }
        else if ((slash = strchr(argv[k], '/')) != NULL) {
            struct stat pathcheck;
            if (stat(argv[k], &pathcheck) == 0) {
                if (S_ISDIR(pathcheck.st_mode))
                    index[path_cnt++] = k;
            }
        }
    }
    if ((path_cnt == 0) && (index != NULL)) {
        free(index);
        return 0;
    }
    *index_out = index;
    return path_cnt;
}

static char **
copy_args(int argc, char **argv, int *mfu_argc, int *copy_len)
{
    int    i, bytes_copied = 0;
    int    check_mfu_args = 1;
    char **argv_copy      = (char **)MFU_MALLOC((size_t)argc * sizeof(char **));
    assert(argv_copy);
    assert(mfu_argc);
    assert(copy_len);
    save_command(argv[0]);

    for (i = 0; i < argc; i++) {
        argv_copy[i] = strdup(argv[i]);
        bytes_copied += (int)(strlen(argv[i]) + 1);
        argv_copy[i] = strdup(argv[i]);
        if (check_mfu_args && (strncmp(argv[i], "-T", 2) == 0)) {
            check_mfu_args = 0;
            *mfu_argc      = i + 1;
        }
    }
    *copy_len = bytes_copied;
    return argv_copy;
}

static int
check_path_count(char *h5tool)
{
    char *thisName;
    char  thistool[1024];
    strcpy(thistool, h5tool);
    thisName = basename(thistool);
    if ((strstr(thisName, "copy") != NULL) || (strstr(thisName, "dump") != NULL) ||
        (strstr(thisName, "format") != NULL) || (strstr(thisName, "ls") != NULL) ||
        (strstr(thisName, "stat") != NULL)) {
        return 1;
    }
    else if ((strstr(thisName, "copy") != NULL) || (strstr(thisName, "diff") != NULL) ||
             (strstr(thisName, "repack") != NULL)) {
        return 2;
    }
    else if ((strstr(thisName, "import") != NULL) || (strstr(thisName, "jam") != NULL)) {
        return 3;
    }
    return -1;
}

int MFU_PRED_EXEC(mfu_flist flist, uint64_t idx, void *arg);
int MFU_PRED_PRINT(mfu_flist flist, uint64_t idx, void *arg);

int
MFU_PRED_EXEC(mfu_flist flist, uint64_t idx, void *arg)
{
    /* get file name for this item */
    int         file_substituted = 0;
    const char *fname            = mfu_flist_file_get_name(flist, idx);

    char *toolname = NULL;
    char  filepath[1024];

    size_t b_offset;

    /* get pointer to encoded argc count and argv array */
    int * count_ptr = arg;
    char *buf       = (char *)arg + sizeof(int);

    /* get number of argv parameters */
    int k = 0, count = *count_ptr;
    toolname = buf;

    /* Get a copy of fname */
    strcpy(filepath, fname);

    /* allocate a char* for each item in the argv array,
     * plus one more for a trailing NULL
     * 'count' in this case is the number of args, so
     * so we add (+1) for the toolname and another (+1)
     * for the trailing NULL to terminate the list
     */

    char   cmdline[2048];
    char **argv = (char **)MFU_CALLOC((size_t)(count + 2), sizeof(char *));
    argv[k++]   = strdup(toolname);

    memset(cmdline, 0, sizeof(cmdline));
    buf += strlen(toolname) + 1;
    /* Reconstruct the command line that the user provided for the h5tool */
    for (k = 1; k < count; k++) {
        if (buf[0] == '&') {
            char *     fname_arg    = NULL;
            void *     check_ptr[2] = {NULL, NULL};
            mfu_flist *check_flist  = memcpy(&check_ptr[0], &buf[1], sizeof(mfu_flist *));
            mfu_flist  flist_arg    = (mfu_flist)check_ptr[0];

            /* +2 (see below) accounts for the '&' and the trailing zero pad */
            buf += sizeof(mfu_flist *) + 2;
            fname_arg = mfu_flist_file_get_name(flist_arg, idx);
            if (fname_arg == NULL) {
                printf("[%d] Warning: Unable to resolve file_substitution %d (idx=%ld)\n", sg_mpi_rank,
                       file_substituted, idx);
                argv[k] = strdup(fname);
            }
            else {
                argv[k] = strdup(fname_arg);
                file_substituted++;
            }
        }
        else {
            argv[k] = strdup(buf);
            buf += strlen(argv[k]) + 1;
        }
    }

    sprintf(cmdline, "\n---------\nCommand:");
    b_offset = strlen(cmdline);
    for (k = 0; k < count; k++) {
        sprintf(&cmdline[b_offset], " %s", argv[k]);
        b_offset = strlen(cmdline);
    }
    sprintf(&cmdline[b_offset], "\n");

    if (log_output_in_single_file) {
        pid_t   pid;
        int     pipefd[2];
        buf_t * thisbuft = NULL;
        buf_t **bufs     = buf_cache;
        if (bufs == NULL) {
            bufs = (buf_t **)MFU_CALLOC(buft_max, sizeof(buf_t *));
            assert((bufs != NULL));
            buf_cache = bufs;
#if 0
			if (buft_count == 0) {
                printf("[%d] Initial buf_cache allocation: buft_count=%d\n", sg_mpi_rank, buft_count);
			}
#endif
            bufs[buft_count++] = thisbuft = (buf_t *)MFU_CALLOC(1, sizeof(buf_t));
            assert((thisbuft != NULL));
        }
        else {
            thisbuft = bufs[buft_count - 1];
            assert((thisbuft != NULL));
            /* Check for remaining space in the current buffer */
            /* If none, then create a new buffer */
            if (thisbuft->count == 0) {
                bufs[buft_count++] = thisbuft = (buf_t *)MFU_CALLOC(1, sizeof(buf_t));
            }
        }
        if ((thisbuft->buf == NULL)) {
            thisbuft->buf = MFU_MALLOC(BUFT_SIZE);
            assert((thisbuft->buf != NULL));
            thisbuft->bufsize = BUFT_SIZE;
            thisbuft->count   = BUFT_SIZE;
            thisbuft->dt      = MPI_CHAR;
        }
        if (pipe(pipefd) == -1) {
            perror("pipe");
            exit(EXIT_FAILURE);
        }
        pid = fork();
        if (pid == -1) {
            perror("fork");
            exit(EXIT_FAILURE);
        }
        if (pid == 0) {
            close(pipefd[0]);
            dup2(pipefd[1], fileno(stdout));
            dup2(pipefd[1], fileno(stderr));
            execvp(argv[0], argv);
        }
        else {
            size_t   nbytes;
            size_t   read_bytes = 0;
            uint64_t remaining, offset;
            close(pipefd[1]);
            buf       = thisbuft->buf;
            remaining = thisbuft->count;
            nbytes    = strlen(cmdline);
            offset    = thisbuft->chars;
            /* Record the command line for the log! */
            if (nbytes < remaining) {
                strcpy(&buf[offset], cmdline);
                thisbuft->chars += nbytes;
                thisbuft->count -= nbytes;
                remaining -= nbytes;
            }
            else { /* We're running out of space in the current buffer  */
                char *nextpart;
                strncpy(&buf[offset], cmdline, remaining);
                nextpart        = &cmdline[remaining + 1];
                thisbuft->count = 0;
                thisbuft->chars += remaining;

                /* Create a new read buffer */
#if 0
                printf("[%d] Allocate-1 a new read buffer:: buft_count=%d\n", sg_mpi_rank, buft_count);
#endif
                bufs[buft_count++] = thisbuft = (buf_t *)MFU_CALLOC(1, sizeof(buf_t));
                assert(thisbuft != NULL);
                thisbuft->buf     = MFU_MALLOC(BUFT_SIZE);
                thisbuft->bufsize = BUFT_SIZE;
                thisbuft->dt      = MPI_CHAR;
                /* Copy the remaining cmdline text into the new buffer */
                strcpy(buf, nextpart);
                /* And update our buffer info */
                // thisbuft->chars = strlen(nextpart) +1;
                thisbuft->chars = strlen(nextpart);
                thisbuft->count = BUFT_SIZE - thisbuft->chars;
            }
            offset = thisbuft->chars;
            while ((nbytes = (size_t)read(pipefd[0], &buf[offset], remaining)) > 0) {
                offset += nbytes;
                read_bytes += nbytes;
                remaining -= nbytes;
                if (remaining == 0) {
                    /* update the curent buffer */
                    thisbuft->count = 0;
                    thisbuft->chars += read_bytes;

                    /* Create a new read buffer */
#if 0
                    printf("[%d] Allocate-2 a new read buffer:: buft_count=%d\n", sg_mpi_rank, buft_count);
#endif
                    bufs[buft_count++] = thisbuft = (buf_t *)MFU_CALLOC(1, sizeof(buf_t));
                    assert(thisbuft != NULL);
                    thisbuft->buf     = MFU_MALLOC(BUFT_SIZE);
                    thisbuft->bufsize = BUFT_SIZE;
                    thisbuft->dt      = MPI_CHAR;
                    thisbuft->chars   = BUFT_SIZE;
                    offset            = 0;
                }
            }
            close(pipefd[0]);
            wait(NULL);

            thisbuft->count = remaining;
            thisbuft->chars = thisbuft->bufsize - remaining;
        }
    }
    else if (log_stdout_in_file) {
        pid_t pid;
        char  logpath[2048];
        char  current_dir[2048];
        char *logbase = strdup(basename(filepath));
        char *thisapp = strdup(basename(toolname));
        if (txtlog == NULL)
            sprintf(logpath, "%s/%s_%s.log", getcwd(current_dir, sizeof(current_dir)), logbase, thisapp);
        else {
            size_t log_len = strlen(txtlog);
            if (txtlog[log_len - 1] == '/')
                sprintf(logpath, "%s%s_%s.log", txtlog, logbase, thisapp);
            else
                sprintf(logpath, "%s/%s_%s.log", txtlog, logbase, thisapp);
        }
        if (mfu_debug_level == MFU_LOG_VERBOSE) {
            printf("\tCreating logfile: %s\n", logpath);
            fflush(stdout);
        }
        pid = fork();
        if (pid == 0) {
            int fd = open(logpath, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
            dup2(fd, fileno(stdout));
            dup2(fd, fileno(stderr));
            close(fd);
            execvp(argv[0], argv);
        }
        int status;
        pid = wait(&status);
        if (logbase)
            free(logbase);
        if (thisapp)
            free(thisapp);
    }

    mfu_free(argv);

    return 0;
}

int
MFU_PRED_PRINT(mfu_flist flist, uint64_t idx, void *arg)
{
    const char *name = mfu_flist_file_get_name(flist, idx);
    printf("%s\n", name);
    return 1;
}

static void
pred_commit(mfu_pred *p)
{
    int need_print = 1;

    mfu_pred *cur = p;
    while (cur) {
        if (cur->f == MFU_PRED_PRINT || cur->f == MFU_PRED_EXEC) {
            need_print = 0;
            break;
        }
        cur = cur->next;
    }
}

int
main(int argc, char **argv)
{
    int i;
    int rc = 0;

    /* initialize MPI */
    MPI_Init(&argc, &argv);
    mfu_init();

    /* get our rank and the size of comm_world */
    int rank, ranks;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ranks);

    /* Assign the static global mpi_rank (for debugging) */
    sg_mpi_rank = rank;
    /* pointer to mfu_walk_opts */
    mfu_walk_opts_t *walk_opts = mfu_walk_opts_new();

#ifdef DAOS_SUPPORT
    /* DAOS vars */
    daos_args_t *daos_args = daos_args_new();
#endif
    int    args_byte_length = -1;
    int    mfu_argc         = argc;
    char * args_buf         = NULL;
    char **h5tool_argv      = copy_args(argc, argv, &mfu_argc, &args_byte_length);

    char *inputname    = NULL;
    char *outputname   = NULL;
    char *sortfields   = NULL;
    char *distribution = NULL;
    char *h5toolname   = NULL;

    int file_histogram = 0;
    int walk           = 0;
    int print          = 0;
    int text           = 0;
    int h5tool_argc    = 0;

    struct distribute_option option;

    // mfu_debug_level = MFU_LOG_WARN; // MFU_LOG_VERBOSE;
    mfu_debug_level = MFU_LOG_WARN;

    /* The struct option declaration can found in bits/getopt_ext.h
     * I've reproduced it here:
     * struct option { char * name; int has_arg; int *flag; int val};
     */
    int                  option_index   = 0;
    static struct option long_options[] = {{"input", required_argument, 0, 'i'},
                                           {"output", required_argument, 0, 'o'},
                                           {"error", optional_argument, 0, 'E'},
                                           {"tool", required_argument, 0, 'T'},
                                           {"log_text", optional_argument, 0, 'l'},
                                           {"help", no_argument, 0, 'h'},
                                           {0, 0, 0, 0}};

    int       usage           = 0;
    int       tool_selected   = 0;
    int       tool_paths      = 0;
    int       tool_args_start = -1;
    int       last_mfu_arg    = 0;
    mfu_pred *pred_head       = NULL;

    while (!tool_selected) {
        int c = getopt_long(mfu_argc + 1, h5tool_argv, "h:i:o:E::T:l::", long_options, &option_index);
        if (c == -1) {
            break;
        }

        switch (c) {
            case 'i':
                inputname    = MFU_STRDUP(optarg);
                last_mfu_arg = optind;
                break;
            case 'o':
                outputname   = MFU_STRDUP(optarg);
                last_mfu_arg = optind;
                if (outputname) {
                    log_output_in_single_file = 1;
                    output_log_file           = strdup(optarg);
                    text                      = 1;
                }
                break;
            case 'E':
                log_errors_in_file = 1;
                errlog             = MFU_STRDUP(optarg);
                last_mfu_arg       = optind;
                break;
            case 'l':
                log_stdout_in_file = 1;
                if (optarg)
                    txtlog = MFU_STRDUP(optarg);
                break;
            case 'T':
                h5toolname = MFU_STRDUP(optarg);
                /* We need to stop parsing user options at this point.
                 * all remaining arguments should be utilized as the
                 * arguments to the selected HDF5 tools.
                 * We also want to avoid any misinterpretations if
                 * HDF5 tool options conflict with the MFU options.
                 */
                // tool_paths      = check_path_count(h5toolname);
                tool_selected   = 1;
                tool_args_start = mfu_argc;
                h5tool_argc     = argc - mfu_argc;
                last_mfu_arg    = optind;
                /* Don't allow any further parsing of arguments */
                break;
            case 'h':
                usage = 1;
                goto show_help;
                break;
            case '?':
                usage = 1;
                break;
            default:
                if (rank == 0) {
                    printf("?? getopt returned character code 0%o ??\n", c);
                }
        }
    }

    /* check that we got a valid progress value */
    if (mfu_progress_timeout < 0) {
        if (rank == 0) {
            MFU_LOG(MFU_LOG_ERR, "Seconds in --progress must be non-negative: %d invalid",
                    mfu_progress_timeout);
        }
        usage = 1;
    }

show_help:
    /* print usage if we need to */
    if (usage) {
        if (rank == 0) {
            print_usage();
        }
        mfu_finalize();
        MPI_Finalize();
        return 1;
    }
    /**************************************************************/
    /* We might consider doing a tool specific argument checking  */
    /* to prevent runtime errors.  We would also like to allow    */
    /* the same command line interface for parallel invocations   */
    /* so that users don't get confused.  Effectively, we should  */
    /* strip out all MFU related arguments and retain copies of   */
    /* everything else to pass into a serial instance of the tool */
    /*                                                            */
    /* As we move forward, we might allow the HDF5 tool to be     */
    /* queried for an acceptable set set of runtime arguments.    */
    /* This could be just a simple string to allow getopt_long    */
    /* to be invoked on the remaing command line arguments.       */
    /**************************************************************/

    int *path_indices = NULL;
    int  numpaths     = count_dirpaths(argc, tool_args_start + 1, argv, &path_indices);

    char **argpaths = NULL;

    /* store src and dest path strings */
    const char *path1         = NULL;
    const char *path2         = NULL;
    size_t      pathlen_total = 0;

    if (numpaths && path_indices) {
        argpaths = &argv[path_indices[0]];
    }
    /* pointer to mfu_file src and dest objects */
    /* The dst object will only be used for tools which
     * accept 2 (or more?) file arguments */
    mfu_file_t *mfu_src_file = NULL;
    mfu_file_t *mfu_dst_file = NULL;

    /* first item is source and second is dest */
    mfu_param_path *srcpath  = NULL;
    mfu_param_path *destpath = NULL;
    mfu_param_path *paths    = NULL;

    mfu_flist flist1 = NULL;
    mfu_flist flist2 = NULL;

    /* allocate structure to define walk options */

    if (numpaths > 0) {
        /* got a path to walk */
        walk = 1;

        /* allocate space for each path */
        paths = (mfu_param_path *)MFU_MALLOC((size_t)numpaths * sizeof(mfu_param_path));

        mfu_src_file = mfu_file_new();

        /* process each path */
        mfu_param_path_set_all((uint64_t)numpaths, (const char **)argpaths, paths, mfu_src_file, true);

        /* don't allow user to specify input file with walk */
        if (inputname != NULL) {
            usage = 1;
        }
    }
    else {
        /* if we're not walking, we must be reading,
         * and for that we need a file */
        if (inputname == NULL) {
            if (rank == 0) {
                MFU_LOG(MFU_LOG_ERR, "Either a <path> or --input is required.");
            }
            usage = 1;
        }
    }

    if (usage) {
        if (rank == 0) {
            print_usage();
        }
#ifdef DAOS_SUPPORT
        daos_cleanup(daos_args, mfu_file, NULL);
#endif
        mfu_file_delete(&mfu_src_file);
        mfu_finalize();
        MPI_Finalize();
        return 0;
    }

    if (numpaths > 0) {
        flist1  = mfu_flist_new();
        srcpath = &paths[0];
        path1   = srcpath->path;
        pathlen_total += strlen(path1);
        mfu_flist_walk_param_paths(1, srcpath, walk_opts, flist1, mfu_src_file);
    }
    if (numpaths > 1) {
        flist2       = mfu_flist_new();
        mfu_dst_file = mfu_file_new();
        destpath     = &paths[1];
        path2        = destpath->path;
        pathlen_total += strlen(path2);
        mfu_flist_walk_param_paths(1, destpath, walk_opts, flist2, mfu_dst_file);
    }

    if (tool_selected && (args_byte_length > 0)) {
        pred_head = mfu_pred_new();
        args_buf  = (char *)MFU_MALLOC(args_byte_length + pathlen_total);
    }

    /* filter files to only include hdf5 files */
    if (flist1) {
        filter_hdf_files(&flist1, NULL, 0, 0);
    }
    if (flist2) {
        filter_hdf_files(&flist2, NULL, 0, 0);
    }
    fflush(stdout);

    if (args_buf != NULL) {
        int   k          = 0;
        char *ptr        = args_buf + sizeof(int);
        char *cmdline    = ptr;
        *(int *)args_buf = h5tool_argc;
        for (i = tool_args_start; i < argc; i++) {
            if (i == path_indices[k]) {
                /* The '&' indicates that what follows is a pointer */
                *ptr++ = '&';
                /* Select which argument list should be used */
                if (k == 0) {
                    memcpy(ptr, &flist1, sizeof(mfu_flist *));
                }
                if (k == 1) {
                    memcpy(ptr, &flist2, sizeof(mfu_flist *));
                }
                ptr += sizeof(mfu_flist *);
                k++;
            }
            else {
                strcpy(ptr, argv[i]);
                ptr += strlen(argv[i]);
            }
            *ptr++ = 0;
        }
        *ptr++ = 0;

        mfu_pred_add(pred_head, MFU_PRED_EXEC, (void *)args_buf);
        pred_commit(pred_head);
    }

    /* apply predicates to each item in list */
    mfu_flist flist3 = mfu_flist_filter_pred(flist1, pred_head);

    /* print details for individual files */
    if (print) {
        mfu_flist_print(flist1);
    }

    /* print summary statistics of flist */
    mfu_flist_print_summary(flist1);

    /* write data to cache file */
    if (outputname != NULL) {
        if (!text) {
            if (rank == 0) {
                puts("ouput capture needs to be a text formated file");
            }
        }
        else {
            dh5tool_flist_write_text(outputname, flist1);
        }
    }

#ifdef DAOS_SUPPORT
    daos_cleanup(daos_args, mfu_file, NULL);
#endif

    /* free users, groups, and files objects */
    mfu_flist_free(&flist1);
    if (flist2)
        mfu_flist_free(&flist2);
    if (flist3)
        mfu_flist_free(&flist3);

    /* free memory allocated for options */
    mfu_free(&distribution);
    mfu_free(&sortfields);
    mfu_free(&outputname);
    mfu_free(&inputname);

    /* free the path parameters */
    mfu_param_path_free_all((uint64_t)numpaths, paths);

    /* free memory allocated to hold params */
    mfu_free(&paths);

    /* free the walk options */
    mfu_walk_opts_delete(&walk_opts);

    /* delete file object */
    mfu_file_delete(&mfu_src_file);

    /* shut down MPI */
    mfu_finalize();
    MPI_Finalize();

    return rc;
}
