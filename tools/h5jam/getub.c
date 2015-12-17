/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

void parse_command_line(int argc, const char *argv[]);

/* Name of tool */
#define PROGRAM_NAME "getub"
char *nbytes = NULL;

static const char *s_opts = "c:";  /* add more later ? */
static struct long_options l_opts[] = {
  {"c", require_arg, 'c'},  /* input file */
  {NULL, 0, '\0'}
};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
usage (const char *prog)
{
    HDfflush(stdout);
    HDfprintf(stdout, "usage: %s -c nb file] \n", prog);
    HDfprintf(stdout, "           print first 'nb' byts of file to stdoug.\n");
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *-------------------------------------------------------------------------
 */
void
parse_command_line(int argc, const char *argv[])
{
    int opt;

    /* parse command line options */
    while((opt = get_option (argc, argv, s_opts, l_opts)) != EOF) {
        switch((char) opt) {
        case 'c':
            nbytes = HDstrdup(opt_arg);
            break;
        case '?':
        default:
            usage(h5tools_getprogname());
            HDexit(EXIT_FAILURE);
        } /* end switch */
    } /* end while */

    if(argc <= opt_ind) {
      error_msg("missing file name\n");
      usage(h5tools_getprogname());
      HDexit(EXIT_FAILURE);
    } /* end if */
} /* end parse_command_line() */

int
main(int argc, const char *argv[])
{
    int fd = -1;
    unsigned size;
    char *filename = NULL;
    long res;
    char *buf = NULL;

    h5tools_setprogname(PROGRAM_NAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    parse_command_line(argc, argv);

    if(NULL == nbytes) {
        /* missing arg */
        error_msg("missing size\n");
        usage(h5tools_getprogname());
        goto error;
    } /* end if */

    if(argc <= (opt_ind)) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        goto error;
    } /* end if */

    filename = HDstrdup(argv[opt_ind]);

    size = 0;
    if(EOF == (res = sscanf(nbytes, "%u", &size))) {
      /* fail */
      error_msg("missing file name\n");
      usage(h5tools_getprogname());
      goto error;
    } /* end if */

    if((fd = HDopen(filename, O_RDONLY, 0)) < 0) {
        error_msg("can't open file %s\n", filename);
        goto error;
    } /* end if */

    if(NULL == (buf = (char *)HDmalloc((unsigned)(size + 1)))) {
        error_msg("can't allocate buffer \n");
        goto error;
    } /* end if */

    res = HDread(fd, buf, (unsigned)size);
    if(res < (long)size) {
        error_msg("Bad read \n");
        goto error;
    } /* end if */

    if(HDwrite(1, buf, (unsigned)size) < 0) {
        error_msg("Bad write \n");
        goto error;
    } /* end if */

    /* close things and exit */
    HDfree(buf);
    HDclose(fd);

    return EXIT_SUCCESS;

error:
    if(buf)
        HDfree(buf);
    if(fd > -1)
        HDclose(fd);
    return EXIT_FAILURE;
} /* end main() */

