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
 * "Server" application to associate a Mirror VFD Driver with a Writer.
 *
 * Server waits on a dedicated port for a Driver to attempt to connect.
 * When connection is made, reads a message from the Driver.
 * If message is "SHUTDOWN", Server closes connection and terminates.
 * Else, if it receives an encoded OPEN xmit (from Driver), the Server forks
 * itself; the child becomes a dedicated Writer and maintains connection with
 * the Driver instance, and the parent process remains a Server and returns
 * to listening for incoming requests.
 * Else, the message is not recognized and is ignored.
 *
 *
 *
 * mirror_server [args]
 *
 * Primary server for coordinating mirror VFD connections with the remote
 * process.
 *
 * args:
 * --help, -h     Print help message and exit.
 * --port=N       Positive integer for primary listening port.
 * --verbosity=N  Debugging verbosity
 *                0: none
 *                1: errors
 *                2: details
 *                3: all
 * --logpath=S    File path to direct debugging output, if any.
 *                Default of none prints output to stdout.
 *
 */

#include "mirror_remote.h"

#ifdef H5_HAVE_MIRROR_VFD

#define LISTENQ      80   /* max pending mirrorS requests              */
#define DEFAULT_PORT 3000 /* default listening port                    */

/* semi-unique "magic" numbers to sanity-check structure pointers */
#define OP_ARGS_MAGIC    0xCF074379u
#define SERVER_RUN_MAGIC 0x741B459Au

/* ---------------------------------------------------------------------------
 * Structure:   struct op_args
 *
 * Purpose:     Convenience structure for holding arguments from command-line.
 *
 * `magic` (uint32_t)
 *      Semi-unique number to help validate a pointer to this struct type.
 *      Must be OP_ARGS_MAGIC to be considered valid.
 *
 * `help` (int)
 *      Flag that the help argument was present in the command line.
 *
 * `main_port` (int)
 *      Flag that the help argument was present in the command line.
 *
 * `verbosity` (int)
 *      Number between 0 (none) and 4 (all) that controls how much detail
 *      the program prints as a course of logging.
 *
 * `log_prepend_serv` (int)
 *      Flag that the logging messages should have 'S- ' at the start of each
 *      line.
 *
 * `log_prepend_type` (int)
 *      Flag that the logging messages should have the associated verbosity
 *      level present in the line (e.g., "WARN", "ERROR", or "INFO").
 *
 * `log_path` (char *)
 *      Path string from the command line, giving the absolute path
 *      for the file for logging output. Can be empty.
 *
 * `writer_log_path` (char *)
 *      Path string from the command line, giving the absolute path
 *      for the file for writer's logging output. Can be empty.
 *
 * ---------------------------------------------------------------------------
 */
struct op_args {
    uint32_t     magic;
    int          help;
    int          main_port;
    unsigned int verbosity;
    int          log_prepend_serv;
    int          log_prepend_type;
    char         log_path[PATH_MAX + 1];
    char         writer_log_path[PATH_MAX + 1];
};

/* ---------------------------------------------------------------------------
 * Structure:   struct server_run
 *
 * Purpose:     Convenience structure for holding information about a server
 *              in operation.
 *
 * `magic` (uint32_t)
 *      Semi-unique number to help validate a pointer to this struct type.
 *      Must be SERVER_RUN_MAGIC to be considered valid.
 *
 * `log_stream` (FILE *)
 *      File handle where logging output is directed.
 *      By default, is stdout.
 *
 * `opts` (struct opt_args)
 *      Contained structure, holds the server's configuration.
 *
 * `listenfd` (int)
 *      File descriptor of the listening socket.
 *
 * ---------------------------------------------------------------------------
 */
struct server_run {
    uint32_t                magic;
    struct op_args          opts;
    struct mirror_log_info *loginfo;
    int                     listenfd;
};

/* ---------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message to stdout.
 * ---------------------------------------------------------------------------
 */
static void
usage(void)
{
    fprintf(stdout,
            "mirror_server [options]\n"
            "\n"
            "Application for providing Mirror Writer process to "
            " Mirror VFD on file-open.\n"
            "Listens on a dedicated socket; forks as a Writer upon receipt"
            " of a valid OPEN xmit.\n"
            "\n"
            "Options:\n"
            "--help [-h]         : Print this help message and quit.\n"
            "--logpath=PATH      : File path for logging output "
            "(default none, to stdout).\n"
            "--port=PORT         : Primary port (default %d).\n"
            "--verbosity=NUM     : Debug printing level "
            "0..4, (default %d).\n",
            DEFAULT_PORT, MIRROR_LOG_DEFAULT_VERBOSITY);
} /* end usage() */

/* ---------------------------------------------------------------------------
 * Function:    parse_args
 *
 * Purpose:     Read command line options and store results in args_out
 *              structure. Fails in event of unrecognized option.
 *
 * Return:      0 on success, -1 on failure.
 * ---------------------------------------------------------------------------
 */
static int
parse_args(int argc, char **argv, struct op_args *args_out)
{
    /* Preset default values */
    args_out->main_port        = DEFAULT_PORT;
    args_out->help             = 0;
    args_out->log_prepend_serv = 1;
    args_out->log_prepend_type = 1;
    args_out->verbosity        = MIRROR_LOG_DEFAULT_VERBOSITY;

    /* Preset empty strings */
    memset(args_out->log_path, 0, PATH_MAX + 1);
    memset(args_out->writer_log_path, 0, PATH_MAX + 1);

    if (argv == NULL || *argv == NULL) {
        mirror_log(NULL, V_ERR, "invalid argv pointer");
        return -1;
    }

    /* Loop over arguments after program name */
    for (int i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "-h", 3) || !strncmp(argv[i], "--help", 7)) {
            mirror_log(NULL, V_INFO, "found help argument");
            args_out->help = 1;
            return 0;
        } /* end if help */
        else if (!strncmp(argv[i], "--port=", 7)) {
            mirror_log(NULL, V_INFO, "parsing 'main_port' (%s)", argv[i] + 7);
            args_out->main_port = atoi(argv[i] + 7);
        } /* end if port */
        else if (!strncmp(argv[i], "--verbosity=", 12)) {
            mirror_log(NULL, V_INFO, "parsing 'verbosity' (%s)", argv[i] + 12);
            args_out->verbosity = (unsigned int)atoi(argv[i] + 12);
        } /* end if verbosity */
        else if (!strncmp(argv[i], "--logpath=", 10)) {
            mirror_log(NULL, V_INFO, "parsing 'logpath' (%s)", argv[i] + 10);
            strncpy(args_out->log_path, argv[i] + 10, PATH_MAX);
        } /* end if logpath */
        else {
            mirror_log(NULL, V_ERR, "unrecognized argument: %s", argv[i]);
            return -1;
        } /* end if unrecognized argument */
    }     /* end for each arg after the path to writer "receiver process" */

    mirror_log(NULL, V_INFO, "all args parsed");

    return 0;
} /* end parse_args() */

/* ---------------------------------------------------------------------------
 * Function:    prepare_listening_socket
 *
 * Purpose:     Configure and open a socket.
 *              In event of error, attempts to undo its processes.
 *
 * Return:      Success: non-negative (the file descriptor of the socket)
 *              Failure: -1
 * ---------------------------------------------------------------------------
 */
static int
prepare_listening_socket(struct server_run *run)
{
    struct sockaddr_in server_addr;
    int                _true     = 1; /* needed for setsockopt() */
    int                ret_value = -1;
    int                ret       = 0; /* for checking return value of function calls */

    if (run == NULL || run->magic != SERVER_RUN_MAGIC) {
        mirror_log(NULL, V_ERR, "invalid server_run pointer");
        return -1;
    }

    mirror_log(run->loginfo, V_INFO, "preparing socket");

    server_addr.sin_family      = AF_INET;
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    server_addr.sin_port        = htons((uint16_t)run->opts.main_port);

    mirror_log(run->loginfo, V_INFO, "socket()");
    ret_value = socket(AF_INET, SOCK_STREAM, 0);
    if (ret_value < 0) {
        mirror_log(run->loginfo, V_ERR, "listening socket:%d", ret_value);
        goto error;
    }

    mirror_log(run->loginfo, V_ALL, "setsockopt()");
    setsockopt(ret_value, SOL_SOCKET, SO_REUSEADDR, &_true, sizeof(int));

    mirror_log(run->loginfo, V_INFO, "bind()");
    ret = bind(ret_value, (struct sockaddr *)&server_addr, sizeof(server_addr));
    if (ret < 0) {
        mirror_log(run->loginfo, V_ERR, "bind() %s", strerror(errno));
        goto error;
    }

    mirror_log(run->loginfo, V_INFO, "listen()");
    ret = listen(ret_value, LISTENQ);
    if (ret < 0) {
        mirror_log(run->loginfo, V_ERR, "H5FD server listen:%d", ret);
        goto error;
    }

    return ret_value;

error:
    if (ret_value >= 0) {
        HDshutdown(ret_value, SHUT_RDWR);
        HDclose(ret_value);
    }
    return -1;
} /* end prepare_listening_socket() */

/* ---------------------------------------------------------------------------
 * Function:    init_server_run
 *
 * Purpose:     Set up server_run struct with default and specified values.
 *
 * Return:      Zero (0) if successful, -1 if an error occurred.
 * ---------------------------------------------------------------------------
 */
static struct server_run *
init_server_run(int argc, char **argv)
{
    struct server_run *run;

    run = (struct server_run *)malloc(sizeof(struct server_run));
    if (run == NULL) {
        mirror_log(NULL, V_ERR, "can't allocate server_run struct");
        return NULL;
    }

    run->magic      = (uint32_t)SERVER_RUN_MAGIC;
    run->opts.magic = (uint32_t)OP_ARGS_MAGIC;
    run->listenfd   = -1;

    if (parse_args(argc, argv, &(run->opts)) < 0) {
        mirror_log(NULL, V_ERR, "can't parse arguments");
        usage();
        goto error;
    }

    if (run->opts.help) {
        usage();
        return run; /* early exit */
    }

    run->loginfo = mirror_log_init(run->opts.log_path, "s- ", run->opts.verbosity);

    run->listenfd = prepare_listening_socket(run);
    if (run->listenfd < 0) {
        mirror_log(NULL, V_ERR, "can't prepare listening socket");
        goto error;
    }

    return run;

error:
    if (run != NULL) {
        free(run);
    }
    return NULL;

} /* end init_server_run() */

/* ---------------------------------------------------------------------------
 * Function:    term_server_run
 *
 * Purpose:     Close opened items in a sever_run and release the pointer.
 *
 * Return:      Zero (0) if successful, -1 if an error occurred.
 * ---------------------------------------------------------------------------
 */
static int
term_server_run(struct server_run *run)
{
    if (run == NULL || run->magic != SERVER_RUN_MAGIC) {
        mirror_log(NULL, V_ERR, "invalid server_run pointer");
        return -1;
    }

    mirror_log(run->loginfo, V_INFO, "shutting down");

    if (run->listenfd >= 0) {
        HDshutdown(run->listenfd, SHUT_RDWR); /* TODO: error-checking? */
        HDclose(run->listenfd);               /* TODO: error-checking? */
        run->listenfd = -1;
    }

    if (mirror_log_term(run->loginfo) < 0) {
        mirror_log(NULL, V_ERR, "can't close logging stream");
        return -1; /* doesn't solve the problem, but informs of error */
    }
    run->loginfo = NULL;

    (run->magic)++;
    (run->opts.magic)++;
    free(run);
    return 0;
} /* end term_server_run() */

/* ---------------------------------------------------------------------------
 * Function:    accept_connection
 *
 * Purpose:     Main working loop; process requests as they are received.
 *              Does nothing if the run option help is set.
 *
 * Return:      -1 on error, else a non-negative file descriptor of the socket.
 * ---------------------------------------------------------------------------
 */
static int
accept_connection(struct server_run *run)
{
    struct sockaddr_in client_addr;      /**/
    socklen_t          clilen;           /**/
    struct hostent    *host_port = NULL; /**/
    char              *hostaddrp;        /**/
    int                connfd = -1;      /* connection file descriptor */

    if (run == NULL || run->magic != SERVER_RUN_MAGIC) {
        mirror_log(NULL, V_ERR, "invalid server_run pointer");
        return -1;
    }

    /*------------------------------*/
    /* accept a connection on a socket */
    clilen = sizeof(client_addr);
    connfd = accept(run->listenfd, (struct sockaddr *)&client_addr, &clilen);
    if (connfd < 0) {
        mirror_log(run->loginfo, V_ERR, "accept:%d", connfd);
        goto error;
    }
    mirror_log(run->loginfo, V_INFO, "connection achieved");

    /*------------------------------*/
    /* get client address information */
    host_port = gethostbyaddr((const char *)&client_addr.sin_addr.s_addr, sizeof(client_addr.sin_addr.s_addr),
                              AF_INET);
    if (host_port == NULL) {
        mirror_log(run->loginfo, V_ERR, "gethostbyaddr()");
        goto error;
    }

    /* function has the string space statically scoped -- OK until next call */
    hostaddrp = inet_ntoa(client_addr.sin_addr);
    /* TODO? proper error-checking */

    mirror_log(run->loginfo, V_INFO, "server connected with %s (%s)", host_port->h_name, hostaddrp);

    return connfd;

error:
    if (connfd >= 0) {
        close(connfd);
    }
    return -1;
} /* end accept_connection() */

/* ---------------------------------------------------------------------------
 * Function:    wait_for_child
 *
 * Purpose:     Signal handler to reap zombie processes.
 * ---------------------------------------------------------------------------
 */
static void
wait_for_child(int H5_ATTR_UNUSED sig)
{
    while (waitpid(-1, NULL, WNOHANG) > 0)
        ;
} /* end wait_for_child() */

/* ---------------------------------------------------------------------------
 * Function:    handle_requests
 *
 * Purpose:     Main working loop; process requests as they are received.
 *              Does nothing if the run option `help` is set.
 *
 * Return:      -1 on error, else 0 for successful operation.
 * ---------------------------------------------------------------------------
 */
static int
handle_requests(struct server_run *run)
{
    int                      connfd = -1;
    char                    *mybuf  = NULL;
    ssize_t                  ret; /* general-purpose error-checking */
    int                      pid; /* process ID of fork */
    struct sigaction         sa;
    H5FD_mirror_xmit_open_t *xopen     = NULL;
    int                      ret_value = 0;

    if (run == NULL || run->magic != SERVER_RUN_MAGIC) {
        mirror_log(NULL, V_ERR, "invalid server_run pointer");
        return -1;
    }

    if (run->opts.help) {
        return 0;
    }

    if (run->listenfd < 0) {
        mirror_log(NULL, V_ERR, "invalid listening socket");
        return -1;
    }

    /* Set up the signal handler */
    sa.sa_handler = wait_for_child;
    HDsigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    if (HDsigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        return 1;
    }

    if (NULL == (mybuf = malloc(H5FD_MIRROR_XMIT_OPEN_SIZE * sizeof(char)))) {
        mirror_log(NULL, V_ERR, "out of memory");
        goto error;
    }
    if (NULL == (xopen = malloc(sizeof(H5FD_mirror_xmit_open_t)))) {
        mirror_log(NULL, V_ERR, "out of memory");
        goto error;
    }

    /* Keep listening for attempts to connect.
     */

    while (1) { /* infinite loop, exited via break or goto */
        mirror_log(run->loginfo, V_INFO, "server waiting for connections...");

        connfd = -1;

        connfd = accept_connection(run);
        if (connfd < 0) {
            mirror_log(run->loginfo, V_ERR, "unable to receive connection");
            goto error;
        }

        /* Read handshake from port connection.
         */

        if ((ret = HDread(connfd, mybuf, H5FD_MIRROR_XMIT_OPEN_SIZE)) < 0) {
            mirror_log(run->loginfo, V_ERR, "read:%d", ret);
            goto error;
        }
        mirror_log(run->loginfo, V_INFO, "received %d bytes", ret);
        mirror_log(run->loginfo, V_ALL, "```");
        mirror_log_bytes(run->loginfo, V_ALL, (size_t)ret, (const unsigned char *)mybuf);
        mirror_log(run->loginfo, V_ALL, "```");

        /* Respond to handshake message.
         */

        if (!strncmp("SHUTDOWN", mybuf, 8)) {
            /* Stop operation if told to stop */
            mirror_log(run->loginfo, V_INFO, "received SHUTDOWN!", ret);

            /* Confirm operation */
            if ((ret = HDwrite(connfd, "CLOSING", 8)) < 0) {
                mirror_log(run->loginfo, V_ERR, "write:%d", ret);
                HDclose(connfd);
                connfd = -1;
                goto error;
            }

            HDclose(connfd);
            connfd = -1;
            goto done;
        } /* end if explicit "SHUTDOWN" directive */
        if (!strncmp("CONFIRM", mybuf, 7)) {
            /* Confirm operation */
            if ((ret = HDwrite(connfd, "ALIVE", 6)) < 0) {
                mirror_log(run->loginfo, V_ERR, "write:%d", ret);
                goto error;
            }
            HDclose(connfd);
        } /* end if "CONFIRM" directive */
        else if (H5FD_MIRROR_XMIT_OPEN_SIZE == ret) {

            mirror_log(run->loginfo, V_INFO, "probable OPEN xmit received");

            H5FD_mirror_xmit_decode_open(xopen, (const unsigned char *)mybuf);
            if (false == H5FD_mirror_xmit_is_open(xopen)) {
                mirror_log(run->loginfo, V_WARN, "expected OPEN xmit was malformed");
                HDclose(connfd);
                continue;
            }

            mirror_log(run->loginfo, V_INFO, "probable OPEN xmit confirmed");

            pid = fork();
            if (pid < 0) { /* fork error */
                mirror_log(run->loginfo, V_ERR, "cannot fork");
                goto error;
            }                    /* end if fork error */
            else if (pid == 0) { /* child process (writer side of fork) */
                mirror_log(run->loginfo, V_INFO, "executing writer");
                if (run_writer(connfd, xopen) < 0) {
                    printf("can't run writer\n");
                }
                else {
                    printf("writer OK\n");
                }
                HDclose(connfd);

                exit(EXIT_SUCCESS);
            }      /* end if writer side of fork */
            else { /* parent process (server side of fork) */
                mirror_log(run->loginfo, V_INFO, "tidying up from handshake");
                HDclose(connfd);
            } /* end if server side of fork */

        } /* end else-if valid request for service */
        else {
            /* Ignore unrecognized messages */
            HDclose(connfd);
            continue;
        } /* end else (not a valid message, to be ignored) */

    } /* end while listening for new connections */

done:
    if (connfd >= 0) {
        mirror_log(run->loginfo, V_WARN, "connfd still open upon cleanup");
        HDclose(connfd);
    }

    free(mybuf);
    free(xopen);

    return ret_value;

error:
    if (connfd >= 0) {
        HDclose(connfd);
    }
    free(mybuf);
    free(xopen);
    return -1;
} /* end handle_requests() */

/* ------------------------------------------------------------------------- */
int
main(int argc, char **argv)
{
    struct server_run *run;

    run = init_server_run(argc, argv);
    if (NULL == run) {
        mirror_log(NULL, V_ERR, "can't initialize run");
        exit(EXIT_FAILURE);
    }

    if (handle_requests(run) < 0) {
        mirror_log(run->loginfo, V_ERR, "problem handling requests");
    }

    if (term_server_run(run) < 0) {
        mirror_log(NULL, V_ERR, "problem closing server run");
        exit(EXIT_FAILURE);
    }

    exit(EXIT_SUCCESS);
} /* end main() */

#else /* H5_HAVE_MIRROR_VFD */

int
main(void)
{
    printf("Mirror VFD was not built -- cannot launch server.\n");
    exit(EXIT_FAILURE);
}

#endif /* H5_HAVE_MIRROR_VFD */
