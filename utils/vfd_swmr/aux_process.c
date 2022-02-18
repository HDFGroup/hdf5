#include "hdf5.h"

#if defined(H5_HAVE_AUX_PROCESS) && !defined(H5_HAVE_WIN32_API)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>

#define FILE_NAME_LEN                  1024
#define SIGNATURE_LEN                  4
#define UPDATER_SIGNATURE              "VUDH"
#define CL_SIGNATURE                   "VUCL"
#define CREATE_METADATA_FILE_ONLY_FLAG 0x0001
#define FINAL_UPDATE_FLAG              0x0002

/* The length for the header of the updater file is 48 bytes, with the breakdown as below:
 *     signature:          4
 *     version:            2
 *     flags:              2
 *     page size:          4
 *     sequence number:    8
 *     tick number:        8
 *     change list offset: 8
 *     change list length: 8
 *     checksum:           4
 */
#define UD_HEADER_LEN 48

/* The length for the top fields of the change list in the updater file is 48 bytes, with the breakdown as below:
 *     signature:                                          4
 *     tick number:                                        8
 *     page offset for metadata file header in updater:    4
 *     length for metadata file header:                    4
 *     checksum for metadata file header:                  4
 *     page offset for metadata file index in updater:     4 
 *     offset for metadata file index in metadata file:    8 
 *     length for metadata file index:                     4
 *     checksum for metadata file index:                   4
 *     number of change list entries:                      4
 */
#define UD_CL_TOP_LEN 48

/* The length for the entry of the change list in the updater file is 20 bytes, with the breakdown as below:
 *     page offset in updater:                             4
 *     page offset in metadata file:                       4
 *     page offset in HDF5 file:                           4
 *     length:                                             4
 *     checksum:                                           4
 */ 
#define CL_ENTRY_LEN 20

/* These decoding macros are borrowed directly from the HDF5 library for making this program stand-alone in the future */
#define UINT16DECODE(p, i)                                                                                   \
    {                                                                                                        \
        (i) = (uint16_t)(*(p)&0xff);                                                                         \
        (p)++;                                                                                               \
        (i) |= (uint16_t)((*(p)&0xff) << 8);                                                                 \
        (p)++;                                                                                               \
    }

#define UINT32DECODE(p, i)                                                                                   \
    {                                                                                                        \
        (i) = (uint32_t)(*(p)&0xff);                                                                         \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p)&0xff) << 8);                                                                 \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p)&0xff) << 16);                                                                \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p)&0xff) << 24);                                                                \
        (p)++;                                                                                               \
    }

#define UINT64DECODE(p, n)                                                                                   \
    {                                                                                                        \
        /* WE DON'T CHECK FOR OVERFLOW! */                                                                   \
        size_t _i;                                                                                           \
                                                                                                             \
        n = 0;                                                                                               \
        (p) += 8;                                                                                            \
        for (_i = 0; _i < sizeof(uint64_t); _i++)                                                            \
            n = (n << 8) | *(--p);                                                                           \
        (p) += 8;                                                                                            \
    }

/* These checksum macros are borrowed directly from the HDF5 library for making this program stand-alone in the future */
#define lookup_rot(x, k) (((x) << (k)) ^ ((x) >> (32 - (k))))

#define lookup_mix(a, b, c)                                                                                  \
    {                                                                                                        \
        a -= c;                                                                                              \
        a ^= lookup_rot(c, 4);                                                                               \
        c += b;                                                                                              \
        b -= a;                                                                                              \
        b ^= lookup_rot(a, 6);                                                                               \
        a += c;                                                                                              \
        c -= b;                                                                                              \
        c ^= lookup_rot(b, 8);                                                                               \
        b += a;                                                                                              \
        a -= c;                                                                                              \
        a ^= lookup_rot(c, 16);                                                                              \
        c += b;                                                                                              \
        b -= a;                                                                                              \
        b ^= lookup_rot(a, 19);                                                                              \
        a += c;                                                                                              \
        c -= b;                                                                                              \
        c ^= lookup_rot(b, 4);                                                                               \
        b += a;                                                                                              \
    }

#define lookup_final(a, b, c)                                                                                \
    {                                                                                                        \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 14);                                                                              \
        a ^= c;                                                                                              \
        a -= lookup_rot(c, 11);                                                                              \
        b ^= a;                                                                                              \
        b -= lookup_rot(a, 25);                                                                              \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 16);                                                                              \
        a ^= c;                                                                                              \
        a -= lookup_rot(c, 4);                                                                               \
        b ^= a;                                                                                              \
        b -= lookup_rot(a, 14);                                                                              \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 24);                                                                              \
    }


typedef struct {
    char     *log_file_path;            /* path name for the log file                                                   */
    FILE     *log_file;                 /* log file containing the details of this program                              */
    FILE     *output;                   /* output the details of this program to STDOUT or a log file                   */
    int      polls_per_tick;            /* number of times to poll for a new updater file per tick (default is 10)      */
    bool     print_stats;               /* print out the stats for this program                                         */
    bool     verbose;                   /* print out the details of this program                                        */
    bool     skip_aux;                  /* skip this program in the case of VDS across multiple files (not implemented) */
    int      tick_len;                  /* tick length in tenths of a second (default is 4)                             */
    char     *vfd_config;               /* configuration string for the VFD stack to be used (default is sec2)          */
    char     *updater_path;             /* path name for the updater files                                              */
    char     *md_file_path;             /* path name for the metadata file                                              */
    char     *md_chksum_path;           /* path name for file containing the checksum values for the metadata file      */
    FILE     *md_file;                  /* pointer to the metadata file                                                 */
    FILE     *md_chksum_file;           /* pointer to the file containing the checksum values for the metadata file     */
    unsigned int  num_mdfile_checksums; /* number of checksum values for the metadata file                              */
    uint32_t      *md_file_checksums;   /* list of checksum values for the metadata file                                */
} handler_t;

/* Structure for the entry of change list in the updater file */
typedef struct {
    void     *data;                     /* buffer for the data (changes)                                                */
    uint32_t ud_file_page_offset;       /* page offset of the data in the updater file                                  */
    uint32_t md_file_page_offset;       /* page offset of the the data in the metadata file                             */
    uint32_t h5_file_page_offset;       /* page offset of the data in the HDF5 file (future usage)                      */
    uint32_t length;                    /* length of the data                                                           */
    uint32_t checksum;                  /* checksum value of the data                                                   */
} cl_entry_t;

/* updater file header related fields */
typedef struct {
    FILE     *file;
    unsigned char     ud_header_buf[UD_HEADER_LEN];
    unsigned char     ud_cl_top_buf[UD_CL_TOP_LEN];
    unsigned char     *cl_buf;

    /* updater file header related fields */
    char     header_signature[5];
    uint16_t version;
    uint16_t flags;
    uint32_t page_size;
    uint64_t sequence_num;
    uint64_t tick_num;
    uint64_t change_list_offset;
    uint64_t change_list_len;
    uint32_t received_header_checksum;
    uint32_t verified_header_checksum;

    /* Updater file change list related fields. */
    char     cl_signature[5];
    uint64_t cl_tick_num;

    uint32_t md_file_header_ud_page_offset; 
    uint32_t md_file_header_len;
    uint32_t md_file_header_chksum;
    void     *md_file_header_buf;

    uint32_t md_file_index_ud_page_offset;
    uint64_t md_file_index_md_file_offset;
    uint32_t md_file_index_len;
    uint32_t md_file_index_chksum;
    void     *md_file_index_buf;

    uint32_t received_cl_checksum;
    uint32_t verified_cl_checksum;

    uint32_t num_cl_entries;
    cl_entry_t *change_list;
    uint32_t cl_chksum; 
} updater_t;

enum aux_arg_level {
    no_arg = 0,  /* doesn't take an argument     */
    require_arg, /* requires an argument          */
    optional_arg /* argument is optional         */
};

/*
 * aux_get_options is a copy of the H5_get_options in hdf5/src/H5system.c.
 *
 * It supports both POSIX and Windows systems.
 * It determines which options are specified on the command line and
 * returns a pointer to any arguments possibly associated with the option in
 * the ``aux_optarg'' variable. aux_get_options returns the shortname equivalent of
 * the option. The long options are specified in the following way:
 *
 * struct aux_long_options foo[] = {
 *   { "filename", require_arg, 'f' },
 *   { "append", no_arg, 'a' },
 *   { "width", require_arg, 'w' },
 *   { NULL, 0, 0 }
 * };
 *
 * Long named options can have arguments specified as either:
 *
 *   ``--param=arg'' or ``--param arg''
 *
 * Short named options can have arguments specified as either:
 *
 *   ``-w80'' or ``-w 80''
 *
 * and can have more than one short named option specified at one time:
 *
 *   -aw80
 *
 * in which case those options which expect an argument need to come at the
 * end.
 */
typedef struct {
    const char *      name;     /* Name of the long option */
    enum aux_arg_level has_arg;  /* Whether we should look for an arg */
    char              shortval; /* The shortname equivalent of long arg
                                 * this gets returned from get_option
                                 */
} aux_long_options;


int         aux_opterr = 1; /* Get_option prints errors if this is on */
int         aux_optind = 1; /* Token pointer                          */
const char *aux_optarg;     /* Flag argument (or value)               */

/*-------------------------------------------------------------------------
 * Function: aux_get_options
 *
 * Purpose:  Determine the command-line options a user specified. We can
 *           accept both short and long type command-lines.
 *
 * Return:  Success:    The short valued "name" of the command line
 *                      parameter or EOF if there are no more
 *                      parameters to process.
 *
 *          Failure:    A question mark.
 *-------------------------------------------------------------------------
 */
int
aux_get_options(int argc, char **argv, const char *opts, const aux_long_options *l_opts)
{
    static int sp      = 1;   /* character index in current token */
    int        optchar = '?'; /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (aux_optind >= argc || argv[aux_optind][0] != '-' || argv[aux_optind][1] == '\0') {
            return EOF;
        }
        else if (strcmp(argv[aux_optind], "--") == 0) {
            aux_optind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[aux_optind][0] == '-' && argv[aux_optind][1] == '-') {
        /* long command line option */
        int        i;
        const char ch      = '=';
        char *     arg     = strdup(&argv[aux_optind][2]);
        size_t     arg_len = 0;

        aux_optarg = strchr(&argv[aux_optind][2], ch);
        arg_len   = strlen(&argv[aux_optind][2]);
        if (aux_optarg) {
            arg_len -= strlen(aux_optarg);
            aux_optarg++; /* skip the equal sign */
        }
        arg[arg_len] = 0;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            if (strcmp(arg, l_opts[i].name) == 0) {
                /* we've found a matching long command line flag */
                optchar = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (aux_optarg == NULL) {
                        if (l_opts[i].has_arg != optional_arg) {
                            if (aux_optind < (argc - 1))
                                if (argv[aux_optind + 1][0] != '-')
                                    aux_optarg = argv[++aux_optind];
                        }
                        else if (l_opts[i].has_arg == require_arg) {
                            if (aux_opterr)
                                fprintf(stderr, "%s: option required for \"--%s\" flag\n", argv[0], arg);

                            optchar = '?';
                        }
                    }
                }
                else {
                    if (aux_optarg) {
                        if (aux_opterr)
                            fprintf(stderr, "%s: no option required for \"%s\" flag\n", argv[0], arg);

                        optchar = '?';
                    }
                }
                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (aux_opterr)
                fprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            optchar = '?';
        }

        aux_optind++;
        sp = 1;

        free(arg);
    }
    else {
        register char *cp; /* pointer into current token */

        /* short command line option */
        optchar = argv[aux_optind][sp];

        if (optchar == ':' || (cp = strchr(opts, optchar)) == 0) {
            if (aux_opterr)
                fprintf(stderr, "%s: unknown option \"%c\"\n", argv[0], optchar);

            /* if no chars left in this token, move to next token */
            if (argv[aux_optind][++sp] == '\0') {
                aux_optind++;
                sp = 1;
            }
            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[aux_optind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                aux_optarg = &argv[aux_optind++][sp + 1];
            }
            else if (++aux_optind >= argc) {
                if (aux_opterr)
                    fprintf(stderr, "%s: value expected for option \"%c\"\n", argv[0], optchar);

                optchar = '?';
            }
            else {
                /* flag value is next token */
                aux_optarg = argv[aux_optind++];
            }

            sp = 1;
        }
        /* wildcard argument */
        else if (*cp == '*') {
            /* check the next argument */
            aux_optind++;
            /* we do have an extra argument, check if not last */
            if ((aux_optind + 1) < argc) {
                if (argv[aux_optind][0] != '-') {
                    aux_optarg = argv[aux_optind++];
                }
                else {
                    aux_optarg = NULL;
                }
            }
            else {
                aux_optarg = NULL;
            }
        }
        else {
            /* set up to look at next char in token, next time */
            if (argv[aux_optind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                aux_optind++;
                sp = 1;
            }
            aux_optarg = NULL;
        }
    }

    /* return the current flag character found */
    return optchar;
}

/*-------------------------------------------------------------------------
 * Function: do_sleep
 *
 * Purpose:  Sleep a time of TICK_LEN / POLLS_PER_TICK
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
do_sleep(handler_t *hand)
{   
    struct timespec time, time2;

    time.tv_sec = (hand->tick_len / hand->polls_per_tick) / 10;
    time.tv_nsec = (hand->tick_len * 100 * 1000 * 1000 / hand->polls_per_tick) % (1000 * 1000 * 1000);
    
    if (nanosleep(&time, &time2) < 0) {
        fprintf(stderr, "Nano sleep system call failed \n");
        return -1;
    }

    return 0; 
}       

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose:  Show command usage
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("    [-h] [-a --skip_aux] [-c --vfd_config] [-l --log_file_path] [-m --md_chksum_path] [-p --polls_per_tick] [-s --stats] [-t --tick_len] [-v --verbose]\n");
    printf("    <md_file> <ud_file>\n");
    printf("    [-h --help]: this help page\n");
    printf("    [-a --skip_aux]: exit if VDS across multiple file is being enabled (to be implemented in the future)\n");
    printf("    [-c --vfd_config]: quoted string containing the configuration string for the VFD stack to be used (default is sec2)\n");
    printf("    [-l --log_file_path]: path to the log file (default is no log file)\n");
    printf("    [-m --md_chksum_path]: path to the file containing the checksum values for testing purpose\n");
    printf("    [-p --polls_per_tick]: number of times to poll for a new updater file per tick (default is 10)\n");
    printf("    [-s --stats]: display stats on exit\n");
    printf("    [-t --tick_len]: integer value indicating the tick length in tenths of a second (default is 4)\n");
    printf("    [-v --verbose]: write log entries to stdout\n");
    printf("    <md_file>: the path to the metadata file. Must be on a POSIX file system.  The file may not exist yet.\n");
    printf("    <ud_file>: the path to the updater files, which end with a number, e.g. updater.1, updater.2, etc.  The path in this example will be /a/b/.../updater.\n");
    printf("\n");
}

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose:  Parse the options that a user specifies
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, char *argv[], handler_t *hand)
{
    int opt;
    aux_long_options long_options[] =
    {
        {"vfd_config=", require_arg, 'c'},
        {"help", no_arg, 'h'},
        {"skip_aux", no_arg, 'a'},
        {"log_file_path=", require_arg, 'l'},
        {"md_file_chksum=", require_arg, 'm'},
        {"polls_per_tick=", require_arg, 'p'},
        {"stats=", require_arg, 's'},
        {"tick_len=", require_arg, 't'},
        {"verbose=", require_arg, 'v'},
        {NULL, 0, 0}
    };

    /* Initialize the command line options */
    hand->log_file_path = NULL;
    hand->log_file = NULL;
    hand->output = NULL;
    hand->polls_per_tick = 10;
    hand->print_stats = false;
    hand->tick_len = 4;
    hand->verbose = false;
    hand->skip_aux = false;
    hand->vfd_config = NULL;
    hand->updater_path = NULL;
    hand->md_file_path = NULL;
    hand->md_chksum_path = NULL;
    hand->md_file = NULL;
    hand->md_chksum_file = NULL;
    hand->num_mdfile_checksums = 0;
    hand->md_file_checksums = NULL;

    if (argc >= 2) {
        hand->updater_path = strdup(argv[argc-1]);
        hand->md_file_path = strdup(argv[argc-2]);
        argc -= 2;
    } else {
        fprintf(stderr, "command-line options must have the path for the metadata file and the updater files\n");
        goto error;
    }

    /* 
     * aux_get_options supports both POSIX and Windows 
     */
    while((opt = aux_get_options(argc, argv, "ac:hl:m:p:st:v", long_options)) != EOF)
    {
        switch(opt)
        {
            case 'a': 
                /* Whether to exit if VDS across multiple files is enabled.  To be implemented in the future */
                hand->skip_aux = true;
                break;
            case 'c':
                /* The configuration string for the VFD stack */
                if(aux_optarg) {
                    fprintf(stdout, "The configuration string for the VFD stack:\t%s\n", aux_optarg);
                    hand->vfd_config = strdup(aux_optarg);
                } else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case 'h': 
                fprintf(stdout, "Help page:\n");
                usage();

                exit(0);

                break;
            case 'l':
                /* The log file */
                if(aux_optarg) {
                    fprintf(stdout, "The log file:\t\t\t\t\t\t%s\n", aux_optarg);
                    hand->log_file_path = strdup(aux_optarg);
                } else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case 'm':
                /* The file with metadata file checksums */
                if(aux_optarg) {
                    fprintf(stdout, "The file with metadata file checksums:\t\t\t%s\n", aux_optarg);
                    hand->md_chksum_path = strdup(aux_optarg);
                } else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case 'p':
                /* The number of polls for the updater per tick */
                if(aux_optarg) {
                    fprintf(stdout, "Number of polls per tick:\t\t\t\t%s\n", aux_optarg);
                    hand->polls_per_tick = atoi(aux_optarg);
                } else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case 's': 
                /* Whether to display stats on exit */
                fprintf(stdout, "Whether to display stats on exit:\t\t\ttrue\n");
                hand->print_stats = true;
                break;
            case 't':
                /* The tick length in tenths of a second */
                if(aux_optarg) {
                    fprintf(stdout, "Tick length (in tenths of a second):\t\t\t%s\n", aux_optarg);
                    hand->tick_len = atoi(aux_optarg);
                } else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case 'v': 
                /* Whether to write log entries to stdout */
                fprintf(stdout, "Whether to write log entries to stdout:\t\t\ttrue\n");
                hand->verbose = true;
                break;
            case ':':
                fprintf(stderr, "option needs a value\n");
                break;
            case '?':
                fprintf(stderr, "unknown option: %c\n", optopt);
                break;
        }
    }

    if (hand->skip_aux) 
        exit(0);

    if (hand->log_file_path) {
        if (!(hand->log_file = fopen(hand->log_file_path, "w"))) {
            fprintf(stderr, "failed to create the log file: %s\n", hand->log_file_path);
            goto error;
        }
   
        hand->output = hand->log_file;
    } else if (hand->verbose) {
        hand->output = stdout;
    }

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: 	checksum_lookup() 
 *
 * Purpose:	checksum_lookup is a copy of the function checksum_lookup3()
 *              in hdf5/src/H5checksum.c
 *
 *              It hashes a variable-length key into a 32-bit value
 *
 * Parameters:  key     : the unaligned variable-length array of bytes
 *              length  : the length of the key, counting by bytes
 *              initval : can be any 4-byte value
 * 
 * Returns:	a 32-bit value.  Every bit of the key affects every bit of
 *              the return value.  Two keys differing by one or two bits
 *              will have totally different hash values.
 *
 * Notes:	The best hash table sizes are powers of 2.  There is no need
 *              to do mod a prime (mod is sooo slow!).  If you need less than
 *              32 bits, use a bitmask.  For example, if you need only 10 bits,
 *              do h = (h & hashmask(10));
 *              In which case, the hash table should have hashsize(10) elements.
 *
 *              If you are hashing n strings (uint8_t **)k, do it like this:
 *              for (i=0, h=0; i<n; ++i) h = H5_checksum_lookup( k[i], len[i], h);
 *
 *              By Bob Jenkins, 2006.  bob_jenkins@burtleburtle.net.  You may
 *              use this code any way you wish, private, educational, or commercial.
 *              It's free.
 *
 *              Use for hash table lookup, or anything where one collision in 2^^32
 *              is acceptable.  Do NOT use for cryptographic purposes.
 *-------------------------------------------------------------------------
 */
static uint32_t
checksum_lookup(const void *key, size_t length, uint32_t initval)
{
    const uint8_t *k = (const uint8_t *)key;
    uint32_t       a, b, c = 0; /* internal state */

    /* Sanity check */
    assert(key);
    assert(length > 0);

    /* Set up the internal state */
    a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;

    /*--------------- all but the last block: affect some 32 bits of (a,b,c) */
    while (length > 12) {
        a += k[0];
        a += ((uint32_t)k[1]) << 8;
        a += ((uint32_t)k[2]) << 16;
        a += ((uint32_t)k[3]) << 24;
        b += k[4];
        b += ((uint32_t)k[5]) << 8;
        b += ((uint32_t)k[6]) << 16;
        b += ((uint32_t)k[7]) << 24;
        c += k[8];
        c += ((uint32_t)k[9]) << 8;
        c += ((uint32_t)k[10]) << 16;
        c += ((uint32_t)k[11]) << 24;
        lookup_mix(a, b, c);
        length -= 12;
        k += 12;
    }

    /*-------------------------------- last block: affect all 32 bits of (c) */
    switch (length) /* all the case statements fall through */
    {
        case 12:
            c += ((uint32_t)k[11]) << 24;
            /* FALLTHROUGH */
        case 11:
            c += ((uint32_t)k[10]) << 16;
            /* FALLTHROUGH */
        case 10:
            c += ((uint32_t)k[9]) << 8;
            /* FALLTHROUGH */
        case 9:
            c += k[8];
            /* FALLTHROUGH */
        case 8:
            b += ((uint32_t)k[7]) << 24;
            /* FALLTHROUGH */
        case 7:
            b += ((uint32_t)k[6]) << 16;
            /* FALLTHROUGH */
        case 6:
            b += ((uint32_t)k[5]) << 8;
            /* FALLTHROUGH */
        case 5:
            b += k[4];
            /* FALLTHROUGH */
        case 4:
            a += ((uint32_t)k[3]) << 24;
            /* FALLTHROUGH */
        case 3:
            a += ((uint32_t)k[2]) << 16;
            /* FALLTHROUGH */
        case 2:
            a += ((uint32_t)k[1]) << 8;
            /* FALLTHROUGH */
        case 1:
            a += k[0];
            break;
        case 0:
            goto done;
        default:
            assert(0 && "This Should never be executed!");
    }

    lookup_final(a, b, c);

done:
    return c;
}

/*-------------------------------------------------------------------------
 * Function: verify_md_file_chksum
 *
 * Purpose:  For testing purpose, make sure the checksum of the metadata
 *           file matches the provided value 
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int 
verify_md_file_chksum(handler_t *hand, uint64_t ud_seq_num)
{
    long int md_file_size = 0;  /* size of the metadata file                 */
    uint32_t verified_checksum; /* calculated checksum for the metadata file */
    void *md_file_buf = NULL;   /* buffer for the entire metadata file       */

    /* Seek the end of the metadata file */
    if (fseek(hand->md_file, 0, SEEK_END) != 0) {
        fprintf(stderr, "failed to seek the end of the metadata file\n");
        goto error;
    }

    /* Find out the size of the metadata file */
    if ((md_file_size = ftell(hand->md_file)) < 0) {
        fprintf(stderr, "failed to find the size of the metadata file\n");
        goto error;
    }

    /* Seek the beginning of the metadata file */
    if (fseek(hand->md_file, 0, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the beginning of the metadata file\n");
        goto error;
    }

    if (md_file_size) {
        md_file_buf = (void *)malloc((unsigned long)md_file_size);

        /* Read the metadata file */
        if (fread(md_file_buf, (size_t)md_file_size, 1, hand->md_file) == 0) {
            fprintf(stderr, "failed to read the metadata file at line %d\n", __LINE__);
            goto error;
        }

        verified_checksum = checksum_lookup(md_file_buf, (size_t)md_file_size, 0);

        if (hand->output) {
            fprintf(hand->output, "\nFor testing, verify the checksum after applying the updater file %llu:\n", ud_seq_num);
            fprintf(hand->output, "received checksum=%u, calculated checksum=%u (at line %d)\n", hand->md_file_checksums[ud_seq_num], verified_checksum, __LINE__);
        }

        /* Make sure the checksum is correct */ 
        if (verified_checksum != hand->md_file_checksums[ud_seq_num]) {
            fprintf(stderr, "received checksum for updater file %llu (%u) doesn't match calculated checksum (%u) at line %d\n",
                ud_seq_num, hand->md_file_checksums[ud_seq_num], verified_checksum, __LINE__);
            goto error;
        }
 
        if (md_file_buf)
            free(md_file_buf);
    }
 
    return 0;

error:
    if (md_file_buf)
        free(md_file_buf);
 
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: decode_ud_header
 *
 * Purpose:  Decode the header of the updater file
 *
 * Return:   Success:    0 (the flag is CREATE_METADATA_FILE_ONLY_FLAG)
 *
 *                       1 (the flag is not CREATE_METADATA_FILE_ONLY_FLAG)
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_ud_header(updater_t *updater, handler_t *hand)
{
    unsigned char         *ptr;

    /* Read the header of the updater file */
    if (fread(updater->ud_header_buf, UD_HEADER_LEN, 1, updater->file) == 0) {
        fprintf(stderr, "failed to read the header of the updater file\n");
        goto error;
    }

    ptr = updater->ud_header_buf;

    /* Check the signature */
    strncpy(updater->header_signature, (char *)ptr, SIGNATURE_LEN);
    updater->header_signature[SIGNATURE_LEN] = '\0';

    if (strcmp(updater->header_signature, UPDATER_SIGNATURE)) {
        fprintf(stderr, "the signature of the updater file is incorrect: %s\n", updater->header_signature);
        goto error;
    }

    /* Check the version number */
    ptr += SIGNATURE_LEN;

    UINT16DECODE(ptr, updater->version);
    
    if (updater->version != 0) {
        fprintf(stderr, "the version of the updater file is incorrect: %hu\n", updater->version);
        goto error;
    }

    /* Get the flag */
    UINT16DECODE(ptr, updater->flags);

    /* Get the page size */
    UINT32DECODE(ptr, updater->page_size);

    /* Get the sequence number */
    UINT64DECODE(ptr, updater->sequence_num);

    /* Get the tick number */
    UINT64DECODE(ptr, updater->tick_num);

    /* Get the offset for the change list */
    UINT64DECODE(ptr, updater->change_list_offset);

    /* Get the length for the change list */
    UINT64DECODE(ptr, updater->change_list_len);

    /* Get the checksum */
    UINT32DECODE(ptr, updater->received_header_checksum);

    /* Verify the checksum for the header */
    updater->verified_header_checksum = checksum_lookup(updater->ud_header_buf, UD_HEADER_LEN - 4, 0);

    /* Compare the checksum */
    if (updater->received_header_checksum != updater->verified_header_checksum) {
        fprintf(stderr, "received header's checksum (%u) doesn't match the calculated one (%u)\n",
            updater->received_header_checksum, updater->verified_header_checksum);
        goto error;
    }

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "header signature=%s\n", updater->header_signature);
        fprintf(hand->output, "version=%hu\n", updater->version);
        fprintf(hand->output, "flags=%hu\n", updater->flags);
        fprintf(hand->output, "page size (bytes)=%u\n", updater->page_size);
        fprintf(hand->output, "sequence number=%llu\n", updater->sequence_num);
        fprintf(hand->output, "tick number=%llu\n", updater->tick_num);
        fprintf(hand->output, "change list offset (bytes)=%llu\n", updater->change_list_offset);
        fprintf(hand->output, "change list length (bytes)=%llu\n", updater->change_list_len);
        fprintf(hand->output, "received checksum for header=%u\n", updater->received_header_checksum);
        fprintf(hand->output, "calculated checksum for header=%u\n\n", updater->verified_header_checksum);
    }

    /* If the flag is CREATE_METADATA_FILE_ONLY_FLAG (0x0001), create the metadata file and close the updater file.
     * Then skip the rest steps and exit the function.  Otherwise, if the metadata file isn't opened yet, it should
     * be the case of opening an existing HDF5 file.  Simply open the metadata file in this case. */
    if (updater->flags & CREATE_METADATA_FILE_ONLY_FLAG) {
        if (!(hand->md_file = fopen(hand->md_file_path, "w+"))) {
            fprintf(stderr, "failed to create the metadata file: %s\n", hand->md_file_path);
            goto error;
        }

        if (fclose(updater->file) == EOF) {
            fprintf(stderr, "updater file close failed\n");
            goto error;
        }

        return 0;
    } else if (!hand->md_file) {
        if (!(hand->md_file = fopen(hand->md_file_path, "w+"))) {
            fprintf(stderr, "failed to create the metadata file: %s\n", hand->md_file_path);
            goto error;
        }

        return 1;
    } else
        return 1;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: decode_cl_top_fields
 *
 * Purpose:  Decode the top part of the change list in the updater file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_cl_top_fields(updater_t *updater, handler_t *hand)
{
    unsigned char         *ptr;

    /*----------------------------------------------
     * Read in the change list and verify
     * the checksum of the change list
     *----------------------------------------------
     */
    updater->cl_buf = (unsigned char *)malloc(updater->change_list_len);

    /* Seek the beginning of the change list in the updater file */
    if (fseek(updater->file, (long)updater->change_list_offset, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the top fields of the change list in the updater file\n");
        goto error;
    }

    /* Read the change list */
    if (fread(updater->cl_buf, updater->change_list_len, 1, updater->file) == 0) {
        fprintf(stderr, "failed to read the top fields of the change list in the updater file\n");
        goto error;
    }

    /* Find the position of the checksum and decode it */
    ptr = updater->cl_buf + updater->change_list_len - 4; 

    UINT32DECODE(ptr, updater->received_cl_checksum);

    /* Calculate the checksum of the change list */
    updater->verified_cl_checksum = checksum_lookup(updater->cl_buf, updater->change_list_len - 4, 0);

    /* Compare the checksum */
    if (updater->received_cl_checksum != updater->verified_cl_checksum) {
        fprintf(stderr, "received change list's checksum (%u) doesn't match the calculated one (%u) for the updater file\n",
            updater->received_cl_checksum, updater->verified_cl_checksum);
        goto error;
    }

    /*----------------------------------------------
     * Decode the top fields of the change list
     *----------------------------------------------
     */
    ptr = updater->cl_buf;

    /* Check the signature */
    strncpy(updater->cl_signature, (char *)ptr, SIGNATURE_LEN);
    updater->cl_signature[SIGNATURE_LEN] = '\0';

    if (strcmp(updater->cl_signature, CL_SIGNATURE)) {
        fprintf(stderr, "the signature of the change list in the updater file is incorrect: %s\n", updater->cl_signature);
        goto error;
    }

    /* Check the tick number */
    ptr += SIGNATURE_LEN;

    /* Get the sequence number */
    UINT64DECODE(ptr, updater->cl_tick_num);

    /* Get the page offset for metadata file header in updater */
    UINT32DECODE(ptr, updater->md_file_header_ud_page_offset);

    /* Get the length for metadata file header */
    UINT32DECODE(ptr, updater->md_file_header_len);

    /* Get the checksum for metadata file header */
    UINT32DECODE(ptr, updater->md_file_header_chksum);

    /* Get the page offset for metadata file header in updater */
    UINT32DECODE(ptr, updater->md_file_index_ud_page_offset);

    /* Get the offset for metadata file index in metadata file */ 
    UINT64DECODE(ptr, updater->md_file_index_md_file_offset);

    /* Get the length for metadata file index */
    UINT32DECODE(ptr, updater->md_file_index_len);

    /* Get the checksum for metadata file index */
    UINT32DECODE(ptr, updater->md_file_index_chksum);

    /* Get the number of change list entries */
    UINT32DECODE(ptr, updater->num_cl_entries);

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "change list signature=%s\n", updater->cl_signature);
        fprintf(hand->output, "change list tick number=%llu\n", updater->cl_tick_num);
        fprintf(hand->output, "page offset for metadata file header in updater=%u\n", updater->md_file_header_ud_page_offset);
        fprintf(hand->output, "length for metadata file header (bytes)=%u\n", updater->md_file_header_len);
        fprintf(hand->output, "checksum for metadata file header=%u\n", updater->md_file_header_chksum);
        fprintf(hand->output, "page offset for metadata file index in updater=%u\n", updater->md_file_index_ud_page_offset);
        fprintf(hand->output, "offset for metadata file index in metadata file (bytes)=%llu\n", updater->md_file_index_md_file_offset);
        fprintf(hand->output, "length for metadata file index (bytes)=%u\n", updater->md_file_index_len);
        fprintf(hand->output, "checksum for metadata file index=%u\n", updater->md_file_index_chksum);
        fprintf(hand->output, "number of change list entries=%u\n", updater->num_cl_entries);
        fprintf(hand->output, "received checksum for the change list=%u\n", updater->received_cl_checksum);
        fprintf(hand->output, "calculated checksum for the change list=%u\n\n", updater->verified_cl_checksum);
    }

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: copy_data
 *
 * Purpose:  Copy data from the source file to the destination file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
copy_data(handler_t *hand, FILE *src_file, FILE *dst_file, uint32_t src_file_offset, uint32_t dst_file_offset, uint32_t data_len, uint32_t received_checksum)
{
    uint32_t verified_checksum;        /* calculated checksum for the data being copied */
    void *data_buf = malloc(data_len); /* buffer for the data being copied              */

    /* Seek and read in the data from the source file */
    if (fseek(src_file, src_file_offset, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the position of the data in the source file\n");
        goto error;
    }

    if (fread(data_buf, data_len, 1, src_file) == 0) {
        fprintf(stderr, "failed to read the data from the source file\n");
        goto error;
    }


    verified_checksum = checksum_lookup(data_buf, data_len, 0);

    /* Compare the checksum */
    if (received_checksum != verified_checksum) {
        fprintf(stderr, "received checksum (%u) doesn't match the calculated one (%u)\n",
            received_checksum, verified_checksum);
        goto error;
    }

    /* Seek and write the data into the destination file */
    if (fseek(dst_file, dst_file_offset, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the position of the data in the destination file\n");
        goto error;
    }

    if (fwrite(data_buf, data_len, 1, dst_file) == 0) {
        fprintf(stderr, "failed to write the data into the destination file\n");
        goto error;
    }

    if (data_buf)
        free(data_buf);

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "\tsource file=%p\n", (void *)src_file);
        fprintf(hand->output, "\tdestination file=%p\n", (void *)dst_file);
        fprintf(hand->output, "\toffset in the source file=%u\n", src_file_offset);
        fprintf(hand->output, "\toffset in the destination file=%u\n", dst_file_offset);
        fprintf(hand->output, "\tlength of data=%u\n", data_len);
        fprintf(hand->output, "\treceived checksum=%u\n", received_checksum);
        fprintf(hand->output, "\tcalculated checksum=%u\n", verified_checksum);
    }

    return 0;
error:
    if (data_buf)
        free(data_buf);
   
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: decode_and_copy_cl_entries
 *
 * Purpose:  Decode the entries of the change list and copy the data 
 *           from the source file to the destination file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_and_copy_cl_entries(updater_t *updater, handler_t *hand)
{
    unsigned char *ptr; /* pointer to the data location */
    unsigned int  i;

    if (updater->num_cl_entries) {
        updater->change_list = (cl_entry_t *)malloc(sizeof(cl_entry_t) * updater->num_cl_entries);

        ptr = updater->cl_buf + UD_CL_TOP_LEN;

        for (i = 0; i < updater->num_cl_entries; i++) {
            UINT32DECODE(ptr, updater->change_list[i].ud_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].md_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].h5_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].length);
            UINT32DECODE(ptr, updater->change_list[i].checksum);

            /* Output the log info */
            if (hand->output) {
                fprintf(hand->output, "change list entry %u\n", i);
                fprintf(hand->output, "\tpage offset of change in updater=%u\n", updater->change_list[i].ud_file_page_offset);
                fprintf(hand->output, "\tpage offset of change in metadata file=%u\n", updater->change_list[i].md_file_page_offset);
                fprintf(hand->output, "\tpage offset of change in HDF5 file=%u\n", updater->change_list[i].h5_file_page_offset);
                fprintf(hand->output, "\tlength of change (bytes)=%u\n", updater->change_list[i].length);
                fprintf(hand->output, "\tchecksum of change=%u\n", updater->change_list[i].checksum);

                fprintf(hand->output, "\ncopy this change to the metadata file:\n");
            }

            if (copy_data(hand, updater->file, hand->md_file, updater->change_list[i].ud_file_page_offset * updater->page_size, 
                updater->change_list[i].md_file_page_offset * updater->page_size, updater->change_list[i].length, updater->change_list[i].checksum) < 0) {
                fprintf(stderr, "failed to copy the data in the change list (%u) from the updater file to the metadata file\n", i);
                goto error;
            }
        }

        if (updater->change_list)
            free(updater->change_list);
    }

    /* Free the buffer for the change list */
    if (updater->cl_buf)
        free(updater->cl_buf);

    return 0;

error:
    if (updater->change_list)
        free(updater->change_list);

    if (updater->cl_buf)
        free(updater->cl_buf);

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: apply_updater
 *
 * Purpose:  Apply the updater file to the metadata file
 *
 * Return:   Success:    true or false (whether close the metadata file)
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
apply_updater(char *updater_name, handler_t *hand)
{
    updater_t updater; /* struct for the updater file header */
    int ret;

    if (hand->output) {
        fprintf(hand->output, "\nupdater_name: %s\n", updater_name);
    }

    /* Open the updater file */
    if (!(updater.file = fopen(updater_name, "r"))) {
        fprintf(stderr, "failed to open the updater file: %s\n", updater_name);
        goto error;
    }

    /*----------------------------------------------
     * Decode the header of the updater file
     *----------------------------------------------
     */
    ret = decode_ud_header(&updater, hand);

    if (ret < 0) {
        fprintf(stderr, "failed to decode the header of the updater file: %s\n", updater_name);
        goto error;
    }

    /* flag for not closing the metadata file (continue to apply updater files) */
    if (ret == false)
        return false;

    /*----------------------------------------------
     * Decode the top fields of the change list
     *----------------------------------------------
     */
    if (decode_cl_top_fields(&updater, hand) < 0) {
        fprintf(stderr, "failed to decode the top fields of the change list: %s\n", updater_name);
        goto error;
    }

    /*
     *----------------------------------------------
     * Decode the actual change list and copy the changes to
     * the metadata file
     *----------------------------------------------
     */
    if (decode_and_copy_cl_entries(&updater, hand) < 0) {
        fprintf(stderr, "failed to decode the change list: %s\n", updater_name);
        goto error;
    }

    /*
     *----------------------------------------------
     * Copy the index and header into the metadata file
     *----------------------------------------------
     */
    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "\ncopy the index to the metadata file:\n");
    }

    if (copy_data(hand, updater.file, hand->md_file, updater.md_file_index_ud_page_offset * updater.page_size, (uint32_t)updater.md_file_index_md_file_offset,
        updater.md_file_index_len, updater.md_file_index_chksum) < 0) {
        fprintf(stderr, "failed to copy the index from the updater file to the metadata file\n");
        goto error;
    }

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "\ncopy the header to the metadata file:\n");
    }

    if (copy_data(hand, updater.file, hand->md_file, updater.md_file_header_ud_page_offset * updater.page_size, 0, updater.md_file_header_len,
        updater.md_file_header_chksum) < 0) {
        fprintf(stderr, "failed to copy the header from the updater file to the metadata file\n");
        goto error;
    }

    /* Make sure the data is in the metadata file */
    if (fflush(hand->md_file) == EOF) {
        fprintf(stderr, "failed to flush the metadata file\n");
        goto error;
    }

    if (fclose(updater.file) == EOF) {
        fprintf(stderr, "updater file close failed\n");
        goto error;
    }

    /*
     *----------------------------------------------
     * Verify the checksum for the metadata file if specified
     *----------------------------------------------
     */
    if (hand->md_chksum_path && verify_md_file_chksum(hand, updater.sequence_num) < 0) {
        fprintf(stderr, "failed in verification of the checksum for the metadata file with the name: %s\n", hand->md_chksum_path);
        goto error;
    }

    /* If the flag is FINAL_UPDATE_FLAG (0x0002), close the metadata file */
    if (updater.flags & FINAL_UPDATE_FLAG) {
        if (fclose(hand->md_file) == EOF) {
            fprintf(stderr, "metadata file close failed\n");
            goto error;
        }

        return true;
    }

    return false;

error:
    /* Free the buffer allocated in decode_cl_top_fields() when error happens */
    if (updater.cl_buf)
        free(updater.cl_buf);

    /* Free the buffer allocated in decode_and_copy_cl_entries() when error happens */
    if (updater.change_list)
        free(updater.change_list);

    fclose(updater.file);

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: get_md_file_chksums
 *
 * Purpose:  Read in the checksum values for the metadata file after applying
 *           each updater file, for later testing purpose.
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
get_md_file_chksums(handler_t *hand)
{
    long int      file_size = 0;
    unsigned char *file_buf = NULL;
    unsigned int  pair_size = 8
                              + 4;
    uint64_t      *seq_nums = NULL;
    unsigned char *ptr      = NULL;
    unsigned int  i;
 
    /* Open the metadata checksum file if it exists */
    if (!(hand->md_chksum_file = fopen(hand->md_chksum_path, "r"))) {
        fprintf(stderr, "failed to open the metadata checksum file: %s\n", hand->md_chksum_path);
        goto error;
    }

    /* Seek the end of the file */
    if (fseek(hand->md_chksum_file, 0, SEEK_END) != 0) {
        fprintf(stderr, "failed to seek the end of the metadata checksum file\n");
        goto error;
    }

    /* Find out the size of the file */
    if ((file_size = ftell(hand->md_chksum_file)) < 0) {
        fprintf(stderr, "failed to find the size of the metadata checksum file\n");
        goto error;
    }

    hand->num_mdfile_checksums = ((unsigned int)file_size) / pair_size;

    seq_nums = (uint64_t *)malloc(hand->num_mdfile_checksums * 64);

    /* This buffer is freed in release_resources() in the end of the program */
    hand->md_file_checksums   = (uint32_t *)malloc(hand->num_mdfile_checksums * 32);

    file_buf = (unsigned char *)malloc((unsigned long)file_size);

    /* Seek the beginning of the metadata checksum file */
    if (fseek(hand->md_chksum_file, 0, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the beginning of the metadata chksum file\n");
        goto error;
    }

    /* Read the metadata file */
    if (fread(file_buf, (size_t)file_size, 1, hand->md_chksum_file) == 0) {
        fprintf(stderr, "failed to read the metadata checksum file\n");
        goto error;
    }

    ptr = file_buf;

    if (hand->output)
        fprintf(hand->output, "\nchecksum for metadata file after applying each updater file is enable:\n");

    for (i = 0; i < hand->num_mdfile_checksums; i++) {
        UINT64DECODE(ptr, seq_nums[i]);
        UINT32DECODE(ptr, hand->md_file_checksums[i]);

        if (hand->output)
            fprintf(hand->output, "\tupdater file %llu: checksum=%u\n", seq_nums[i], hand->md_file_checksums[i]);
    }

    if (fclose(hand->md_chksum_file) == EOF) {
        fprintf(stderr, "updater file close failed\n");
        goto error;
    }

    if (seq_nums)
        free(seq_nums);

    if (file_buf)
        free(file_buf);

    return 0;

error:
    if (seq_nums)
        free(seq_nums);

    if (file_buf)
        free(file_buf);

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: check_updater
 *
 * Purpose:  Wait for the updater file to appear, then apply it to the
 *           metadata file.
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
check_updater(handler_t *hand)
{
    int updater_exists;
    int stop_update = 0;
    char updater_name[FILE_NAME_LEN];
    int i;

    /* For testing: retrieve the metadata file checksum values if it exists */
    if (hand->md_chksum_path && get_md_file_chksums(hand) < 0) {
        fprintf(stderr, "failed to get the metadata file checksum with the name: %s\n", hand->md_chksum_path);
        goto error;
    }

    /* Let it loop until an updater file appears */
    for (i = 0; ; i++) {
        sprintf(updater_name, "%s.%d", hand->updater_path, i);

        updater_exists = -1;

        do {
            updater_exists = access(updater_name, F_OK);
     
            do_sleep(hand);
        } while (updater_exists != 0);

        stop_update = apply_updater(updater_name, hand);

        if (stop_update)
            break;
        else if (stop_update < 0)
            goto error;    
    }

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: release_resources
 *
 * Purpose:  Free some memory
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
release_resources(handler_t *hand)
{
    if (hand->log_file_path) {
        free(hand->log_file_path);

        if (fclose(hand->log_file) == EOF) {
           fprintf(stderr, "log file close failed\n");
           goto error;
        }
    }

    if (hand->vfd_config)
        free(hand->vfd_config);
    
    if (hand->updater_path)
        free(hand->updater_path);

    if (hand->md_file_path) 
        free(hand->md_file_path);

    /* This buffer is allocated in get_md_file_chksums() */
    if (hand->md_file_checksums)
        free(hand->md_file_checksums);

    if (hand->md_chksum_path)
        free(hand->md_chksum_path);
  
    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose:  This program (auxiliary process) applies the updater files to
 *           the copy of the metadata file to support NFS file system.
 * 
 * Return:   Success:    0
 *
 *           Failure:    1
 *-------------------------------------------------------------------------
 */
int
main (int argc, char** argv)
{
    handler_t hand;

    if (parse_command_line(argc, argv, &hand) < 0)
        goto error;

    if (check_updater(&hand) < 0)
        goto error;

    if (release_resources(&hand) < 0)
        goto error;

    return 0;

error:
    return 1;
}

#else /* H5_HAVE_AUX_PROCESS */
int
main ()
{
    return 0;
}
#endif /* H5_HAVE_AUX_PROCESS */
