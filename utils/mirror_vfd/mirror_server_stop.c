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
 * Purpose:     Stop the mirror server
 *              Exists for cross-platform, optionally remote shutdown.
 */

#include "H5private.h" /* System compatibility call-wrapper macros */

#ifdef H5_HAVE_MIRROR_VFD

#define MSHS_OPTS_MAGIC     0x613B1C15u /* sanity-checking constant */
#define MSHS_IP_STR_SIZE    20
#define MSHS_DEFAULT_IP     "127.0.0.1"
#define MSHS_DEFAULT_PORTNO 3000

/* ----------------------------------------------------------------------------
 * Structure:   struct mshs_opts
 *
 * Purpose:     Convenience structure to hold options as parsed from the
 *              command line.
 *
 * `magic` (uint32_t)
 *      Semi-unique constant to help verify pointer integrity.
 *
 * `help` (int)
 *      Flag that the help argument was present.
 *
 * `portno` (int)
 *      Port number, as received from arguments.
 *
 * `ip` (char *)
 *      IP address string as received from arguments.
 *
 * ----------------------------------------------------------------------------
 */
struct mshs_opts {
    uint32_t magic;
    int      help;
    int      portno;
    char     ip[MSHS_IP_STR_SIZE + 1];
};

/* ----------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print usage message to stdout.
 * ----------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("mirror_server_stop [options]\n"
           "System-independent Mirror Server shutdown program.\n"
           "Sends shutdown message to Mirror Server at given IP:port\n"
           "\n"
           "Options:\n"
           "    -h | --help Print this usage message and exit.\n"
           "    --ip=ADDR   IP Address of remote server (default %s)\n"
           "    --port=PORT Handshake port of remote server (default %d)\n",
           MSHS_DEFAULT_IP, MSHS_DEFAULT_PORTNO);
} /* end usage() */

/* ----------------------------------------------------------------------------
 * Function:    parse_args
 *
 * Purpose:     Parse command-line arguments, populating the options struct
 *              pointer as appropriate.
 *              Default values will be set for unspecified options.
 *
 * Return:      0 on success, negative (-1) if error.
 * ----------------------------------------------------------------------------
 */
static int
parse_args(int argc, char **argv, struct mshs_opts *opts)
{
    int i = 0;

    opts->magic  = MSHS_OPTS_MAGIC;
    opts->help   = 0;
    opts->portno = MSHS_DEFAULT_PORTNO;
    strncpy(opts->ip, MSHS_DEFAULT_IP, MSHS_IP_STR_SIZE);

    for (i = 1; i < argc; i++) { /* start with first possible option argument */
        if (!strncmp(argv[i], "-h", 3) || !strncmp(argv[i], "--help", 7)) {
            opts->help = 1;
        }
        else if (!strncmp(argv[i], "--ip=", 5)) {
            strncpy(opts->ip, argv[i] + 5, MSHS_IP_STR_SIZE);
        }
        else if (!strncmp(argv[i], "--port=", 7)) {
            opts->portno = atoi(argv[i] + 7);
        }
        else {
            printf("Unrecognized option: '%s'\n", argv[i]);
            usage();
            opts->magic++; /* invalidate for sanity */
            return -1;
        }
    } /* end for each argument from command line */

    /* auto-replace 'localhost' with numeric IP */
    if (!strncmp(opts->ip, "localhost", 10)) { /* include null terminator */
        strncpy(opts->ip, "127.0.0.1", MSHS_IP_STR_SIZE);
    }

    return 0;
} /* end parse_args() */

/* ----------------------------------------------------------------------------
 * Function:    send_shutdown
 *
 * Purpose:     Create socket and send shutdown signal to remote server.
 *
 * Return:      0 on success, negative (-1) if error.
 * ----------------------------------------------------------------------------
 */
static int
send_shutdown(struct mshs_opts *opts)
{
    char               mybuf[16];
    int                live_socket;
    struct sockaddr_in target_addr;

    if (opts->magic != MSHS_OPTS_MAGIC) {
        printf("invalid options structure\n");
        return -1;
    }

    live_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (live_socket < 0) {
        printf("ERROR socket()\n");
        return -1;
    }

    target_addr.sin_family      = AF_INET;
    target_addr.sin_port        = htons((uint16_t)opts->portno);
    target_addr.sin_addr.s_addr = inet_addr(opts->ip);
    memset(target_addr.sin_zero, 0, sizeof(target_addr.sin_zero));

    if (connect(live_socket, (struct sockaddr *)&target_addr, (socklen_t)sizeof(target_addr)) < 0) {
        printf("ERROR connect() (%d)\n%s\n", errno, strerror(errno));
        return -1;
    }

    if (HDwrite(live_socket, "SHUTDOWN", 9) == -1) {
        printf("ERROR write() (%d)\n%s\n", errno, strerror(errno));
        return -1;
    }

    /* Read & verify response from port connection.  */
    if (HDread(live_socket, &mybuf, sizeof(mybuf)) == -1) {
        printf("ERROR read() can't receive data\n");
        return -1;
    }
    if (strncmp("CLOSING", mybuf, 8)) {
        printf("ERROR read() didn't receive data from server\n");
        return -1;
    }

    if (HDclose(live_socket) < 0) {
        printf("ERROR close() can't close socket\n");
        return -1;
    }

    return 0;
} /* end send_shutdown() */

/* ------------------------------------------------------------------------- */
int
main(int argc, char **argv)
{
    struct mshs_opts opts;

    if (parse_args(argc, argv, &opts) < 0) {
        printf("Unable to parse arguments\n");
        exit(EXIT_FAILURE);
    }

    if (opts.help) {
        usage();
        exit(EXIT_FAILURE);
    }

    if (send_shutdown(&opts) < 0) {
        printf("Unable to send shutdown command\n");
        exit(EXIT_FAILURE);
    }

    exit(EXIT_SUCCESS);
} /* end main() */

#else /* H5_HAVE_MIRROR_VFD */

/* ------------------------------------------------------------------------- */
int
main(void)
{
    printf("Mirror VFD not built -- unable to perform shutdown.\n");
    exit(EXIT_FAILURE);
}

#endif /* H5_HAVE_MIRROR_VFD */
