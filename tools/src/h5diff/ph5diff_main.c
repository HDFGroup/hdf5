/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5diff.h"
#include "ph5diff.h"
#include "h5diff_common.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5diff"

static void ph5diff_worker(int);

/* ---------------------------------------------------------------------------
 * MPI_Unpack helpers — mirror of the pack helpers in h5diff.c.
 *
 * Wire format (MPI_PACKED, MPI_TAG_ARGS):
 *   name1  : 1 MPI_INT (len) + len MPI_CHAR
 *   name2  : 1 MPI_INT (len) + len MPI_CHAR
 *   scalars: all int-like and double fields of diff_opt_t (see pack_diff_args)
 *   exclude            : exclude list (see unpack_exclude_list)
 *   exclude_attr       : exclude list
 *   exclude_attr_names : exclude list
 *   sset[0]            : subset (see unpack_sset)
 *   sset[1]            : subset
 *   argdata            : diff_args_t (packed field-by-field)
 *
 * Each exclude list: 1 MPI_INT (count), then for each node:
 *   1 MPI_INT (str len) + len MPI_CHAR
 *
 * Each subset: 1 MPI_INT (has_sset flag); if non-zero:
 *   for each of start/stride/count/block:
 *     1 MPI_INT (nelem) + nelem MPI_UNSIGNED_LONG_LONG
 * --------------------------------------------------------------------------- */

static struct exclude_path_list *
unpack_exclude_list(const void *buf, int bufsiz, int *pos)
{
    struct exclude_path_list *head  = NULL;
    struct exclude_path_list *prev  = NULL;
    int                       count = 0;
    int                       i;

    MPI_CHECK(MPI_Unpack(buf, bufsiz, pos, &count, 1, MPI_INT, MPI_COMM_WORLD));
    for (i = 0; i < count; i++) {
        int                       slen = 0;
        char                     *tmp;
        struct exclude_path_list *node;

        MPI_CHECK(MPI_Unpack(buf, bufsiz, pos, &slen, 1, MPI_INT, MPI_COMM_WORLD));
        tmp = (char *)malloc((size_t)slen + 1);
        if (!tmp)
            break;
        MPI_CHECK(MPI_Unpack(buf, bufsiz, pos, tmp, slen, MPI_CHAR, MPI_COMM_WORLD));
        tmp[slen] = '\0';

        node = (struct exclude_path_list *)malloc(sizeof(*node));
        if (!node) {
            free(tmp);
            break;
        }
        node->obj_path = tmp; /* already heap-allocated above */
        node->obj_type = H5TRAV_TYPE_UNKNOWN;
        node->next     = NULL;
        if (!head)
            head = node;
        else
            prev->next = node;
        prev = node;
    }
    return head;
}

static struct subset_t *
unpack_sset(const void *buf, int bufsiz, int *pos)
{
    int              has_sset = 0;
    struct subset_t *sset;
    hsize_t        **fields[4];
    unsigned int    *lens[4];
    int              f;

    MPI_CHECK(MPI_Unpack(buf, bufsiz, pos, &has_sset, 1, MPI_INT, MPI_COMM_WORLD));
    if (!has_sset)
        return NULL;

    sset = (struct subset_t *)calloc(1, sizeof(*sset));
    if (!sset)
        return NULL;

    /* Unpack start, stride, count, block in order */
    fields[0] = &sset->start.data;
    fields[1] = &sset->stride.data;
    fields[2] = &sset->count.data;
    fields[3] = &sset->block.data;
    lens[0]   = &sset->start.len;
    lens[1]   = &sset->stride.len;
    lens[2]   = &sset->count.len;
    lens[3]   = &sset->block.len;

    for (f = 0; f < 4; f++) {
        int nelem = 0;
        MPI_CHECK(MPI_Unpack(buf, bufsiz, pos, &nelem, 1, MPI_INT, MPI_COMM_WORLD));
        *lens[f] = (unsigned int)nelem;
        if (nelem > 0) {
            *fields[f] = (hsize_t *)malloc((size_t)nelem * sizeof(hsize_t));
            if (*fields[f])
                MPI_CHECK(
                    MPI_Unpack(buf, bufsiz, pos, *fields[f], nelem, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD));
        }
    }
    return sset;
}

static void
free_unpacked_sset(struct subset_t *sset)
{
    if (!sset)
        return;
    free(sset->start.data);
    free(sset->stride.data);
    free(sset->count.data);
    free(sset->block.data);
    free(sset);
}

static void
unpack_diff_args(const void *buf, int bufsiz, struct diff_mpi_args *args)
{
    int pos  = 0;
    int slen = 0;

    /* name1 */
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &slen, 1, MPI_INT, MPI_COMM_WORLD));
    args->name1 = (char *)malloc((size_t)slen + 1);
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, args->name1, slen, MPI_CHAR, MPI_COMM_WORLD));
    args->name1[slen] = '\0';

    /* name2 */
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &slen, 1, MPI_INT, MPI_COMM_WORLD));
    args->name2 = (char *)malloc((size_t)slen + 1);
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, args->name2, slen, MPI_CHAR, MPI_COMM_WORLD));
    args->name2[slen] = '\0';

    /* scalar diff_opt_t fields — must match pack order in pack_diff_args().
     * bool and enum fields are narrowed from int via a local variable to
     * avoid writing 4 bytes into a 1-byte (bool) or potentially-smaller
     * (enum) field. */
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.mode_quiet, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.mode_report, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.mode_verbose, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.mode_verbose_level, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.mode_list_not_cmp, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.print_header, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.print_percentage, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.print_dims, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.delta_bool, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.delta, 1, MPI_DOUBLE, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.use_system_epsilon, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.percent_bool, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.percent, 1, MPI_DOUBLE, MPI_COMM_WORLD));
    {
        int v;
        MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &v, 1, MPI_INT, MPI_COMM_WORLD));
        args->opts.follow_links = (bool)v;
    }
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.no_dangle_links, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.cmn_objs, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.not_cmp, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.contents, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.do_nans, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.disable_compact_subset, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.exclude_path, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.exclude_attr_path, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.exclude_attr_name, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.count_bool, 1, MPI_INT, MPI_COMM_WORLD));
    MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &args->opts.count, 1, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD));
    {
        int v;
        MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &v, 1, MPI_INT, MPI_COMM_WORLD));
        args->opts.err_stat = (diff_err_t)v;
    }

    /* pointer fields: exclude lists */
    args->opts.exclude            = unpack_exclude_list(buf, bufsiz, &pos);
    args->opts.exclude_attr       = unpack_exclude_list(buf, bufsiz, &pos);
    args->opts.exclude_attr_names = unpack_exclude_list(buf, bufsiz, &pos);

    /* pointer fields: sset[2] */
    args->opts.sset[0] = unpack_sset(buf, bufsiz, &pos);
    args->opts.sset[1] = unpack_sset(buf, bufsiz, &pos);

    /* argdata: unpack type[2] and is_same_trgobj via local ints to avoid
     * writing int-sized data into enum/bool fields */
    {
        int t0, t1, same;
        MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &t0, 1, MPI_INT, MPI_COMM_WORLD));
        MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &t1, 1, MPI_INT, MPI_COMM_WORLD));
        MPI_CHECK(MPI_Unpack(buf, bufsiz, &pos, &same, 1, MPI_INT, MPI_COMM_WORLD));
        args->argdata.type[0]        = (h5trav_type_t)t0;
        args->argdata.type[1]        = (h5trav_type_t)t1;
        args->argdata.is_same_trgobj = (bool)same;
    }
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: ph5diff main program
 *
 * Return: An exit status of 0 means no differences were found, 1 means some
 *   differences were found.
 *
 * Comments:
 *
 * This function drives the diff process and will do a serial or parallel diff depending
 * on the value of the global variable g_Parallel (default is 0), set to 1 when the program
 * is run as "ph5diff"
 *-------------------------------------------------------------------------
 */

int
main(int argc, char *argv[])
{
    int         nID      = 0;
    const char *fname1   = NULL;
    const char *fname2   = NULL;
    const char *objname1 = NULL;
    const char *objname2 = NULL;
    diff_opt_t  opts;

    MPI_Init(&argc, (char ***)&argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &nID);
    MPI_Comm_size(MPI_COMM_WORLD, &g_nTasks);

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    outBuffOffset = 0;
    g_Parallel    = 1;

    if (g_nTasks == 1) {
        fprintf(rawerrorstream, "Only 1 task available...doing serial diff\n");

        g_Parallel = 0;

        parse_command_line(argc, (const char *const *)argv, &fname1, &fname2, &objname1, &objname2, &opts);

        h5diff(fname1, fname2, objname1, objname2, &opts);

        print_info(&opts);
    }
    /* Parallel h5diff */
    else {

        /* Manager parses the command line and drives the diff; workers stay
         * in their probe loop until dismissed.  All fields of diff_mpi_args —
         * including the dynamically-allocated exclude lists and sset pointers —
         * are serialized with MPI_Pack into each MPI_TAG_ARGS message, so
         * workers always stay in their probe loop and can receive MPI_TAG_END
         * at any time without a separate communication step. */
        if (nID == 0) {
            parse_command_line(argc, (const char *const *)argv, &fname1, &fname2, &objname1, &objname2,
                               &opts);

            h5diff(fname1, fname2, objname1, objname2, &opts);

            MPI_Barrier(MPI_COMM_WORLD);

            print_manager_output();

            print_info(&opts);
        }
        /* All other tasks become workers and wait for assignments. */
        else {
            ph5diff_worker(nID);

            MPI_Barrier(MPI_COMM_WORLD);
        } /* end else */

    } /* end else */

    MPI_Finalize();

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: ph5diff_worker
 *
 * Purpose: worker process of ph5diff
 *
 * Return: none
 *
 *-------------------------------------------------------------------------
 */
static void
ph5diff_worker(int nID)
{
    hid_t file1_id = H5I_INVALID_HID;
    hid_t file2_id = H5I_INVALID_HID;

    while (1) {
        MPI_Status Status;

        MPI_Probe(0, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);

        /* Check for filenames */
        if (Status.MPI_TAG == MPI_TAG_PARALLEL) {
            char filenames[2][MAX_FILENAME];

            /* Retrieve filenames */
            MPI_Recv(filenames, MAX_FILENAME * 2, MPI_CHAR, 0, MPI_TAG_PARALLEL, MPI_COMM_WORLD, &Status);

            /* disable error reporting */
            H5E_BEGIN_TRY
            {
                /* Open the files */
                if ((file1_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
                    printf("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[0]);
                    MPI_Abort(MPI_COMM_WORLD, 0);
                }
                if ((file2_id = H5Fopen(filenames[1], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
                    printf("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[1]);
                    MPI_Abort(MPI_COMM_WORLD, 0);
                }
                /* enable error reporting */
            }
            H5E_END_TRY
        }
        /* Check for work */
        else if (Status.MPI_TAG == MPI_TAG_ARGS) {
            struct diff_mpi_args args;
            struct diffs_found   diffs;
            unsigned             i;
            int                  msg_size;
            void                *buf;

            /* Make certain we've received the filenames and opened the files already */
            if (file1_id < 0 || file2_id < 0) {
                printf("ph5diff_worker: ERROR: work received before/without filenames\n");
                MPI_Abort(MPI_COMM_WORLD, 0);
                break;
            }

            /* Determine exact message size and receive into a heap buffer. */
            MPI_Get_count(&Status, MPI_PACKED, &msg_size);

            if (NULL == (buf = malloc((size_t)msg_size))) {
                printf("ph5diff_worker: ERROR: malloc failed for recv buffer\n");
                MPI_Abort(MPI_COMM_WORLD, 0);
                break;
            }

            MPI_Recv(buf, msg_size, MPI_PACKED, 0, MPI_TAG_ARGS, MPI_COMM_WORLD, &Status);

            /* Unpack all fields: scalars, then exclude lists, then ssets, then argdata. */
            memset(&args, 0, sizeof(args));
            unpack_diff_args(buf, msg_size, &args);
            free(buf);

            /* Do the diff */
            diffs.nfound  = diff(file1_id, args.name1, file2_id, args.name2, &(args.opts), &(args.argdata));
            diffs.not_cmp = args.opts.not_cmp;

            /* Free heap memory allocated during unpacking.
             * The exclude lists are freed by diff() itself via the serial
             * free_exclude_*_list paths inside diff_match/build_match_list,
             * so we must not free them here. name1/name2 and sset are not
             * touched by those paths and must be freed explicitly. */
            free(args.name1);
            free(args.name2);
            free_unpacked_sset(args.opts.sset[0]);
            free_unpacked_sset(args.opts.sset[1]);

            if ((outBuffOffset == 0) && !overflow_file)
                /* Nothing to print. Send diffs to manager */
                MPI_Send(&diffs, sizeof(diffs), MPI_BYTE, 0, MPI_TAG_DONE, MPI_COMM_WORLD);
            else {
                /*
                 * If print buffer or overflow file have something in
                 * them, request print token.
                 */
                MPI_Send(NULL, 0, MPI_BYTE, 0, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD);

                /* Wait for print token. */
                MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD, &Status);

                if (outBuffOffset > 0) {
                    /* When get token, send all of our output to the manager task and then return the token */
                    for (i = 0; i < outBuffOffset; i += PRINT_DATA_MAX_SIZE)
                        MPI_Send(outBuff + i, PRINT_DATA_MAX_SIZE, MPI_CHAR, 0, MPI_TAG_PRINT_DATA,
                                 MPI_COMM_WORLD);
                }

                /* An overflow file exists, so we send its output to
                 * the manager too and then delete it.
                 */
                if (overflow_file) {
                    char out_data[PRINT_DATA_MAX_SIZE + 1];
                    int  tmp;

                    memset(out_data, 0, PRINT_DATA_MAX_SIZE + 1);
                    i = 0;

                    rewind(overflow_file);
                    while ((tmp = getc(overflow_file)) != EOF) {
                        *(out_data + i++) = (char)tmp;
                        if (i == PRINT_DATA_MAX_SIZE) {
                            MPI_Send(out_data, PRINT_DATA_MAX_SIZE, MPI_CHAR, 0, MPI_TAG_PRINT_DATA,
                                     MPI_COMM_WORLD);
                            i = 0;
                            memset(out_data, 0, PRINT_DATA_MAX_SIZE + 1);
                        }
                    }

                    if (i > 0)
                        MPI_Send(out_data, PRINT_DATA_MAX_SIZE, MPI_CHAR, 0, MPI_TAG_PRINT_DATA,
                                 MPI_COMM_WORLD);

                    fclose(overflow_file);
                    overflow_file = NULL;
                }

                fflush(stdout);
                memset(outBuff, 0, OUTBUFF_SIZE);
                outBuffOffset = 0;

                MPI_Send(&diffs, sizeof(diffs), MPI_BYTE, 0, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD);
            }
        }
        /* Check for leaving */
        else if (Status.MPI_TAG == MPI_TAG_END) {
            MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_END, MPI_COMM_WORLD, &Status);
            break;
        }
        else {
            printf("ph5diff_worker: ERROR: invalid tag (%d) received\n", Status.MPI_TAG);
            MPI_Abort(MPI_COMM_WORLD, 0);
            break;
        }
    }

    H5Fclose(file1_id);
    H5Fclose(file2_id);

    return;
}

/*-------------------------------------------------------------------------
 * Function: print_manager_output
 *
 * Purpose: special function that prints any output accumulated by the
 *      manager task.
 *
 * Return: none
 *
 *-------------------------------------------------------------------------
 */
void
print_manager_output(void)
{
    /* If there was something we buffered, let's print it now */
    if (g_Parallel) {
        if (outBuffOffset > 0)
            printf("%s", outBuff);

        if (overflow_file) {
            int tmp;
            rewind(overflow_file);
            while ((tmp = getc(overflow_file)) != EOF)
                putchar(tmp);
            fclose(overflow_file);
            overflow_file = NULL;
        }

        fflush(stdout);
        memset(outBuff, 0, OUTBUFF_SIZE);
        outBuffOffset = 0;
    }
    else if (outBuffOffset > 0) {
        fprintf(rawerrorstream, "h5diff error: outBuffOffset > 0, but we're not in parallel!\n");
    }
}

/*-------------------------------------------------------------------------
 * Function: h5diff_exit
 *
 * Purpose: dismiss phdiff worker processes and exit
 *
 * Return: none
 *
 *-------------------------------------------------------------------------
 */
void
h5diff_exit(int status)
{
    /* if in parallel mode, dismiss workers, close down MPI, then exit */
    if (g_Parallel) {
        if (g_nTasks > 1) {
            phdiff_dismiss_workers();
            MPI_Barrier(MPI_COMM_WORLD);
        }
    }

    MPI_Finalize();

    status =
        EXIT_SUCCESS; /* Reset exit status, since some mpiexec commands generate output on failure status */

    h5tools_close();

    /* Always exit(0), since MPI implementations do weird stuff when they
     *  receive a non-zero exit value. - QAK
     */
    exit(status);
}
