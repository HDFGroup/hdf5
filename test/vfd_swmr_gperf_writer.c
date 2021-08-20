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

/* Description of this program:
 * This program checks the performance of group creations for VFD SWMR.
 * Currently the group creation time, H5Fopen and H5Fclose time are measured.
 * After compiling the program,
 *     ./vfd_swmr_gperf_writer -n 1000 -P -N 5 -q
 * will generate 1000 groups, each group has 5 attributes.
 *     ./vfd_swmr_gperf_writer -n 1000 -P -N 0 -q
 * will generate 1000 empty groups.
 *     ./vfd_swmr_gperf_writer -n 1000 -P -l 1 -q
 * will generate 1000 groups with 1 level of nested groups,(like /g1/g2)
 *      each group has one attribute.
 *     ./vfd_swmr_gperf_writer -n 1000 -P -S -G -V -N 5 -l 1 -m 8 -t 4 -B 16384 -s 8192
 * will generate 1000 groups with 1 level of nested groups,(like /g1/g2)
 *      each group has 5 attributes and the attribute type is variable length string.
 *      The groups is created without using VFD SWMR;
 *      The groups are created with the earliest file format(old-styled)
 *      The program is run with max_lag = 8, tick_len = 4;
 *      The page buffer size is 16384 bytes. The page size is 8192 bytes.
 *
 */
#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define VS_ATTR_NAME_LEN 21

#define TIME_PASSED(X, Y)                                                                                    \
    ((double)((Y.tv_sec - X.tv_sec) * 1000000000 + (Y.tv_nsec - X.tv_nsec))) / 1000000000.0

typedef struct {
    hid_t        file, filetype, one_by_one_sid;
    char         filename[PATH_MAX];
    char         progname[PATH_MAX];
    unsigned int asteps;
    unsigned int nsteps;
    bool         use_vfd_swmr;
    bool         old_style_grp;
    char         grp_op_pattern;
    bool         grp_op_test;
    char         at_pattern;
    bool         attr_test;
    uint32_t     max_lag;
    uint32_t     tick_len;
    bool         gperf;
    double       min_gc_time;
    double       max_gc_time;
    double       mean_gc_time;
    double       total_gc_time;
    double       total_time;
    double       mean_time;
    double       fo_total_time;
    double       fc_total_time;
    unsigned int num_attrs;
    bool         vlstr_test;
    unsigned int ps;
    unsigned int pbs;
    unsigned int nglevels;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .asteps = 1, .nsteps = 100, .use_vfd_swmr = true,                     \
        .old_style_grp = false, .grp_op_pattern = ' ', .grp_op_test = false, .at_pattern = ' ',              \
        .attr_test = false, .tick_len = 4, .max_lag = 7, .gperf = false, .min_gc_time = 100.,                \
        .max_gc_time = 0., .mean_gc_time = 0., .total_gc_time = 0., .total_time = 0., .mean_time = 0.,       \
        .fo_total_time = 0., .fc_total_time = 0., .num_attrs = 1, .vlstr_test = false, .ps = 4096,           \
        .pbs = 4096, .nglevels = 0                                                                           \
    }

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: ./%s -P -n 1000 -N 5 -q (create 1000 groups, each group has 5 attributes)\n"
            "Options: \n"
            " [-P] [-S] [-G] [-t tick_len] [-m max_lag][-B pbs] [-s ps]\n"
            " [-n ngroups] [-l ng_levels] [-O grp_op_pattern]\n"
            " [-N num_attrs] [-V] [-b] [-A at_pattern] [-a steps] [-q]\n"
            "\n"
            "-P:             carry out the performance test\n"
            "-S:             do not use VFD SWMR\n"
            "-G:             old-style type of group\n"
            "-t tick_len:    length of a tick in tenths of a second.\n"
            "-m max_lag:     maximum expected lag(in ticks) between writer and readers\n"
            "-B pbs:         page Buffer Size in bytes:\n"
            "                The default value is 4K(4096).\n"
            "-s ps:          page size used by page aggregation, page buffer and \n"
            "                the metadata file. \n"
            "-n ngroups:     the number of groups\n"
            "-l ng_levels:   the number of level of nested groups.  \n"
            "                If all the groups are under the root group,  \n"
            "                this number should be 0.\n"
            "-N num_attrs:   the number of attributes \n"
            "-V              use variable length string attributes for performance test\n"
            "-b:             write data in big-endian byte order\n"
            "                (For the performance test, -V overwrites -b)\n"
            "-A at_pattern:  `at_pattern' for different attribute tests\n"
            "              The value of `at_pattern` is one of the following:\n"
            "              `compact`              - Attributes added in compact storage\n"
            "              `dense`                - An attribute added in dense storage\n"
            "              `compact-del`          - Attributes added and then one\n"
            "                                       attribute deleted, in compact \n"
            "              `dense-del`            - Attributes added until the storage\n"
            "                                       is dense then an attribute deleted\n"
            "                                       the storge still in dense\n"
            "              `compact-add-to-dense` - Attributes added first in compact\n"
            "                                       then in dense storage\n"
            "              `dense-del-to-compact` - Attributes added until the storage\n"
            "                                       is dense, then several attributes \n"
            "                                       deleted, the storage changed to\n"
            "                                       compact\n"
            "              `modify`               - An attribute added then modified\n"
            "              `add-vstr`             - A VL string attribute added\n"
            "              `remove-vstr`          - A VL string attribute added then\n"
            "                                       deleted\n"
            "              `modify-vstr`          - A VL string attribute added then \n"
            "                                       modified \n"
            "              `add-ohr-block`        - An attribute is added and this forces\n"
            "                                       the creation of object header\n"
            "                                       continuation block \n"
            "              `del-ohr-block`        - An attribute is added and this forces\n"
            "                                       the creation of object header\n"
            "                                       continuation block and then this \n"
            "                                       attribute is deleted so the \n"
            "                                       object header continuation block is \n"
            "                                       removed. \n"
            "-O grp_op_pattern:  `grp_op_pattern' for different group operation tests\n"
            "              The value of `grp_op_pattern` is one of the following:\n"
            "              `grp-creation`         - A group is created.\n"
            "              `grp-deletion`         - An existing group is deleted.\n"
            "              `grp-move`             - A group is moved to become \n"
            "                                       another group. \n"
            "              `grp-ins-links`        - Links are inserted, including\n"
            "                                       both hard and soft links. \n"
            "              `grp-del-links`        - Links are deleted, including\n"
            "                                       both hard ans soft links. \n"
            "              `grp-compact-t-dense`  - Links are inserted to the group.\n"
            "                                       The link storage of this group \n"
            "                                       changed from compact to dense. \n"
            "                                       The links include both hard and\n"
            "                                       soft links.                    \n"
            "              `grp-dense-t-compact`  - Links are inserted to the group\n"
            "                                       The link storage of this group \n"
            "                                       changed from compact to dense. \n"
            "                                       Then several links are deleted.\n"
            "                                       The link storage changed from  \n"
            "                                       dense to compact again.        \n"
            "                                       The links include both hard and\n"
            "                                       soft links.                    \n"
            "-a steps:      `steps` between adding attributes\n"
            "              (Don't recommend to use this option for performance test.)\n"
            "-q:             silence printouts, few messages\n"
            "\n",
            progname);
    exit(EXIT_FAILURE);
}

static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    const hsize_t dims  = 1;
    char *        tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        printf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        HDfree(tfile);
        tfile = NULL;
    }

    if (argc == 1)
        usage(s->progname);
    while ((ch = getopt(argc, argv, "PSGa:bVt:m:B:s:n:qA:N:l:O:")) != -1) {
        switch (ch) {
            case 'P':
                s->gperf = true;
                break;
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'G':
                s->old_style_grp = true;
                break;
            case 'a':
            case 'n':
            case 'N':
            case 'l':
            case 't':
            case 'm':
            case 'B':
            case 's':
                errno = 0;
                tmp   = HDstrtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    printf("`-%c` argument `%lu` too large\n", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'N')
                    s->num_attrs = (unsigned)tmp;
                else if (ch == 'l')
                    s->nglevels = (unsigned)tmp;
                else if (ch == 't')
                    s->tick_len = (unsigned)tmp;
                else if (ch == 'm')
                    s->max_lag = (unsigned)tmp;
                else if (ch == 's')
                    s->ps = (unsigned)tmp;
                else if (ch == 'B')
                    s->pbs = (unsigned)tmp;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'V':
                s->vlstr_test = true;
                break;
            case 'O':
                if (HDstrcmp(optarg, "grp-creation") == 0)
                    s->grp_op_pattern = 'c';
                else if (HDstrcmp(optarg, "grp-deletion") == 0)
                    s->grp_op_pattern = 'd';
                else if (HDstrcmp(optarg, "grp-move") == 0)
                    s->grp_op_pattern = 'm';
                else if (HDstrcmp(optarg, "grp-ins-links") == 0)
                    s->grp_op_pattern = 'i';
                else if (HDstrcmp(optarg, "grp-del-links") == 0)
                    s->grp_op_pattern = 'D';
                else if (HDstrcmp(optarg, "grp-compact-t-dense") == 0)
                    s->grp_op_pattern = 't';
                else if (HDstrcmp(optarg, "grp-dense-t-compact") == 0)
                    s->grp_op_pattern = 'T';
                else {
                    printf("Invalid -O argument \"%s\"", optarg);
                    TEST_ERROR;
                }
                break;
            case 'A':
                if (HDstrcmp(optarg, "compact") == 0)
                    s->at_pattern = 'c';
                else if (HDstrcmp(optarg, "dense") == 0)
                    s->at_pattern = 'd';
                else if (HDstrcmp(optarg, "compact-add-to-dense") == 0)
                    s->at_pattern = 't';
                else if (HDstrcmp(optarg, "compact-del") == 0)
                    s->at_pattern = 'C';
                else if (HDstrcmp(optarg, "dense-del") == 0)
                    s->at_pattern = 'D';
                else if (HDstrcmp(optarg, "dense-del-to-compact") == 0)
                    s->at_pattern = 'T';
                else if (HDstrcmp(optarg, "modify") == 0)
                    s->at_pattern = 'M';
                else if (HDstrcmp(optarg, "add-vstr") == 0)
                    s->at_pattern = 'v';
                else if (HDstrcmp(optarg, "remove-vstr") == 0)
                    s->at_pattern = 'r';
                else if (HDstrcmp(optarg, "modify-vstr") == 0)
                    s->at_pattern = 'm';
                else if (HDstrcmp(optarg, "add-ohr-block") == 0)
                    s->at_pattern = 'a';
                else if (HDstrcmp(optarg, "del-ohr-block") == 0)
                    s->at_pattern = 'R';
                else {
                    printf("Invalid -A argument \"%s\"", optarg);
                    TEST_ERROR;
                }
                break;
            case 'q':
                verbosity = 0;
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= optind;
    argv += optind;

    if (s->grp_op_pattern != ' ')
        s->grp_op_test = true;
    if (s->at_pattern != ' ')
        s->attr_test = true;

    if (!s->grp_op_test) {
        if (s->asteps < 1 || s->asteps > s->nsteps) {
            printf("attribute interval is out of bounds\n");
            TEST_ERROR;
        }
    }

    if (s->grp_op_test && s->attr_test) {
        printf("Cannot test both group operation and attribute tests!\n");
        printf("Attribute tests are ignored.\n");
    }

    if (argc > 0) {
        printf("unexpected command-line arguments\n");
        TEST_ERROR;
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        printf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    check_ohr_num_chunk
 *
 * Purpose:     Check if the number of object header chunks is as expected.
 *
 * Parameters:  hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              bool one_chunk_ohr
 *              flag to indicate if the object header chunk is 1 or greater
 *              1: true
 *              greater than 1: false
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static bool
check_ohr_num_chunk(hid_t g, bool one_chunk_ohr)
{

    H5O_native_info_t ninfo;

    /* Get the object information */
    if (H5Oget_native_info(g, &ninfo, H5O_NATIVE_INFO_HDR) < 0) {
        printf("H5Oget_native_info failed\n");
        TEST_ERROR;
    }

    if (true == one_chunk_ohr) {
        if (ninfo.hdr.nchunks != 1) {
            printf("Object header should have only one chunk,but it is not.\n");
            TEST_ERROR;
        }
    }
    else {
        if (ninfo.hdr.nchunks <= 1) {
            printf("Object header should have more than one chunk,but it is not.\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_attr
 *
 * Purpose:     Add attributes to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t oid
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *              unsigned num_attrs
 *              The number of attributes to be created
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int g_which
 *              This parameter is used to generate correct group name in a key
 *              debugging message.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static bool
add_attr(state_t *s, hid_t oid, unsigned int which, unsigned num_attrs, const char *aname_fmt,
         unsigned int g_which)
{

    char     attrname[VS_ATTR_NAME_LEN];
    unsigned u;
    unsigned attr_value;
    hid_t    aid    = H5I_INVALID_HID;
    hid_t    amtype = H5I_INVALID_HID;
    hid_t    atype  = s->filetype;
    hid_t    sid    = s->one_by_one_sid;

    /* Need to obtain native datatype for H5Aread */
    if ((amtype = H5Tget_native_type(atype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        /* Construct attribute name like attr-0-0 */
        HDsprintf(attrname, aname_fmt, which, u);
        if ((aid = H5Acreate2(oid, attrname, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            printf("H5Acreate2 failed\n");
            TEST_ERROR;
        }

        attr_value = u + which;

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, g_which, u + which);

        /* Write data into the attribute */
        if (H5Awrite(aid, amtype, &attr_value) < 0) {
            printf("H5Awrite failed\n");
            TEST_ERROR;
        }

        /* Close attribute */
        if (H5Aclose(aid) < 0) {
            printf("H5Aclose failed\n");
            TEST_ERROR;
        }

        /* If coming to an "object header continuation block" test,
         * we need to check if this test behaves as expected. */
        if (s->at_pattern == 'a' || s->at_pattern == 'R') {
            if (false == check_ohr_num_chunk(oid, false)) {
                printf("An object header continuation block should be created. \n");
                printf("But it is not.\n");
                TEST_ERROR;
            }
        }

    } /* end for */

    if (H5Tclose(amtype) < 0) {
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(amtype);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_vlstr_attr
 *
 * Purpose:     Add a variable length string  attribute to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "vstr" test.
 *-------------------------------------------------------------------------
 */

static bool
add_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;
    hid_t sid      = s->one_by_one_sid;

    /* Allocate buffer for the VL string value */
    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Assign the VL string value and the attribute name.. */
    HDsprintf(astr_val, "%u", which);
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Generate the VL string attribute.*/
    if ((aid = H5Acreate2(g, name, atype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Acreate2 failed.\n");
        TEST_ERROR;
    }

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    HDfree(astr_val);

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        HDfree(astr_val);

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_vlstr_attrs
 *
 * Purpose:     Add variable length string attributes to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which".  The attribute names
 *              are "attr-which","attr-which+1"....
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the performance test that has the VL string type.
 *-------------------------------------------------------------------------
 */

static bool
add_vlstr_attrs(state_t *s, hid_t g, unsigned int which, unsigned int num_attrs)
{
    unsigned u;
    bool     ret_value = true;
    for (u = 0; u < num_attrs; u++) {
        ret_value = add_vlstr_attr(s, g, u + which);
        if (ret_value == false)
            break;
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_default_group_attr
 *
 * Purpose:     Add an attribute to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "dense" storage test.
 *              It is also used by the group-only, "add-ohr-block"
 *              and "del-ohr-block" tests.
 *-------------------------------------------------------------------------
 */

static bool
add_default_group_attr(state_t *s, hid_t g, unsigned int which)
{

    const char *aname_format = "attr-%u-%u";

    /* Note: the number of attributes can be configurable,
     * the default number of attribute is 1.
     */
    /* If the vl string attribute type is chosen. */
    if (s->vlstr_test == true)
        return add_vlstr_attrs(s, g, which, s->num_attrs);
    else
        return add_attr(s, g, which, s->num_attrs, aname_format, which);
}

/*-------------------------------------------------------------------------
 * Function:    del_one_attr
 *
 * Purpose:     delete one attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              HDF5 object ID (in this file: means group ID)
 *
 *              bool is_dense
 *              if the deleted attribute is for checking the dense storage
 *
 *              bool is_vl_or_ohrc
 *              if the deleted attribute is a VL string or for object header
 *              continuation check test
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute names
 *              according to if this attribute is a VL string or for checking
 *              the dense storage or the storage transition from dense to
 *              compact.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static bool
del_one_attr(state_t *s, hid_t obj_id, bool is_dense, bool is_vl_or_ohrc, unsigned int which)
{

    char attrname[VS_ATTR_NAME_LEN];

    /*attribute name template for the dense storage related deletion operation */
    const char *aname_format_d = "attr-d-%u-%u";

    /*attribute name template used for general attribute deletion operation */
    const char *aname_format = "attr-%u-%u";

    /*attribute name template used for VL string attribute deletion
     * or object header continuation check operations */
    const char *aname_format_vl = "attr-%u";

    dbgf(2, "writer: coming to delete the attribute.\n");

    /* Construct the attribute name */
    if (is_dense == true)
        HDsprintf(attrname, aname_format_d, which, 0);
    else if (is_vl_or_ohrc == true)
        HDsprintf(attrname, aname_format_vl, which, 0);
    else
        HDsprintf(attrname, aname_format, which, 0);

    /* Delete the attribute */
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete() failed\n");
        TEST_ERROR;
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(obj_id, true)) {
            printf("The object header chunk should not continue. \n");
            TEST_ERROR;
        }
    }
    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_del_vlstr_attr
 *
 * Purpose:     Add a variable length string attribute
 *              then delete this attribute in this a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "remove-vstr" test.
 *-------------------------------------------------------------------------
 */

static bool
add_del_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    bool ret_value = false;

    /* Add a VL string attribute then delete it. */
    ret_value = add_vlstr_attr(s, g, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, false, true, which);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    modify_attr
 *
 * Purpose:     Modify the value of an attribute in a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char*aname_fmt
 *              The attribute name template used to create unique attribute names.
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static bool
modify_attr(state_t *s, hid_t g, const char *aname_fmt, unsigned int which)
{

    char         attrname[VS_ATTR_NAME_LEN];
    hid_t        aid    = H5I_INVALID_HID;
    hid_t        amtype = H5I_INVALID_HID;
    unsigned int modify_value;

    HDsprintf(attrname, aname_fmt, which, 0);
    if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if ((amtype = H5Tget_native_type(s->filetype, H5T_DIR_ASCEND)) < 0) {
        printf("H5Tget_native_type failed\n");
        TEST_ERROR;
    }

    /* Make a large number to verify the change easily */
    modify_value = which + 10000;

    if (H5Awrite(aid, amtype, &modify_value) < 0) {
        printf("H5Awrite failed\n");
        TEST_ERROR;
    }
    if (H5Tclose(amtype) < 0) {
        printf("H5Tclose failed\n");
        TEST_ERROR;
    }
    if (H5Aclose(aid) < 0) {
        printf("H5Aclose failed\n");
        TEST_ERROR;
    }

    return true;
error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(aid);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    modify_vlstr_attr
 *
 * Purpose:     Modify the value of an VL string attribute in a group.
 *
 * Parameters:
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
 */

static bool
modify_vlstr_attr(hid_t g, unsigned int which)
{

    hid_t aid   = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char  name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;

    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        printf("Allocate memory for VL string failed.\n");
        TEST_ERROR;
    }

    /* Change the VL string value and create the attribute name. */
    HDsprintf(astr_val, "%u%c", which, 'A');
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        printf("Cannot create variable length datatype.\n");
        TEST_ERROR;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        printf("Cannot set variable length datatype.\n");
        TEST_ERROR;
    }

    /* Open this attribute. */
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        printf("H5Aopen failed.\n");
        TEST_ERROR;
    }

    dbgf(1, "The modified VL string value  is  %s \n", astr_val);

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        printf("H5Awrite failed.\n");
        TEST_ERROR;
    }

    if (H5Tclose(atype) < 0) {
        printf("H5Tclose() failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        printf("H5Aclose() failed\n");
        TEST_ERROR;
    }

    HDfree(astr_val);

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atype);
    }
    H5E_END_TRY;

    if (astr_val)
        HDfree(astr_val);

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_modify_vlstr_attr
 *
 * Purpose:     Add a variable length string attribute
 *              then modify this attribute in this a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "modify-vstr" test.
 *-------------------------------------------------------------------------
 */

static bool
add_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which)
{

    bool ret_value = false;
    ret_value      = add_vlstr_attr(s, g, which);
    if (true == ret_value)
        ret_value = modify_vlstr_attr(g, which);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact
 *
 * Purpose:     Add some attributes to the group.
 *              the number of attributes should be the maximal number of
 *              attributes that the compact storage can hold
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "modify-vstr" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
add_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-%u-%u";

    if (s->old_style_grp)
        max_compact = 2;
    else {
        /* Obtain the maximal number of attributes to be stored in compact
         * storage and the minimal number of attributes to be stored in
         * dense storage. */
        if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
            printf("H5Pget_attr_phase_change() failed\n");
            TEST_ERROR;
        }
    }

    /* Add max_compact attributes, these attributes are stored in
     * compact storage. */
    return add_attr(s, g, which, max_compact, aname_format, which);

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group.
 *              First, the number of attributes should be the maximal number
 *              of attributes that the compact storage can hold.
 *              Then,  add another atribute, the storage becomes dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "compact-to-dense" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
add_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    unsigned    max_compact  = 0;
    unsigned    min_dense    = 0;
    const char *aname_format = "attr-d-%u-%u";
    bool        ret_value    = false;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    /* Add attributes, until just before converting to dense storage */
    ret_value = add_attrs_compact(s, g, gcpl, which);

    /* Add another attribute, the storage becomes dense. */
    if (ret_value == true)
        ret_value = add_attr(s, g, which + max_compact, 1, aname_format, which);

    return ret_value;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    del_attrs_compact_dense_compact
 *
 * Purpose:     delete some attributes in the group.
 *              The number of attributes are deleted in such a way
 *              that the attribute storage changes from compact to
 *              dense then to compact again.
 *
 * Parameters:  hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by the
 *              "dense-del-to-compact" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
del_attrs_compact_dense_compact(hid_t obj_id, hid_t gcpl, unsigned int which)
{

    unsigned max_compact = 0;
    unsigned min_dense   = 0;
    unsigned u           = 0;

    char        attrname[VS_ATTR_NAME_LEN];
    const char *aname_format  = "attr-%u-%u";
    const char *adname_format = "attr-d-%u-%u";

    /* Obtain the maximal number of attributes to be stored in compact
     * storage and the minimal number of attributes to be stored in
     * dense storage. */
    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }
    u = max_compact + 1;

    /* delete a number of attributes so that the attribute storage just becomes dense.*/
    for (u--; u >= (min_dense - 1); u--) {
        HDsprintf(attrname, aname_format, which, max_compact - u);
        if (H5Adelete(obj_id, attrname) < 0) {
            printf("H5Adelete failed\n");
            TEST_ERROR;
        }
    }

    /* The writer deletes another attribute, the storage is
     * still dense. However, the attribute to be deleted
     * doesn't follow the previous for loop. It may be
     * in different location in the object header. Just add
     * a litter variation to check if this operation is successful.
     * The attribute name to be deleted is attr-max_compact+which-0
     */

    HDsprintf(attrname, adname_format, max_compact + which, 0);
    if (H5Adelete(obj_id, attrname) < 0) {
        printf("H5Adelete failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact
 *
 * Purpose:     Add some attributes to the group and then delete one attribute.
 *              First, the number of attributes to be added should be the
 *              maximal number of attributes that the compact storage can hold.
 *              Then,  delete one atribute, the storage is still compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "compact-del" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
add_del_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    bool ret_value = false;
    ret_value      = add_attrs_compact(s, g, gcpl, which);
    if (ret_value == true) {
        dbgf(2, "writer: before deleting the attribute.\n");
        ret_value = del_one_attr(s, g, false, false, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact_dense
 *
 * Purpose:     Add some attributes to the group and then delete one attribute.
 *              First, the number of attributes to be added exceeds
 *              the maximal number of attributes that the compact storage can hold.
 *              The storage changes from compact to dense.
 *              Then,  delete one atribute, the storage is still dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "dense-del" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
add_del_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    bool     ret_value   = false;
    unsigned max_compact = 0;
    unsigned min_dense   = 0;

    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        printf("H5Pget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    ret_value = add_attrs_compact_dense(s, g, gcpl, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, true, false, which + max_compact);

    return ret_value;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    add_del_attrs_compact_dense_compact
 *
 * Purpose:     Add attributes to the group and then delete some of them.
 *              First, the number of attributes to be added exceeds
 *              the maximal number of attributes that the compact storage can hold.
 *              The storage changes from compact to dense.
 *              Then,  delete some attributes, the storage changes from
 *              dense to compact again.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is for the "dense-del-to-compact" test.
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
 */

static bool
add_del_attrs_compact_dense_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    bool ret_value = false;
    ret_value      = add_attrs_compact_dense(s, g, gcpl, which);
    if (ret_value == true)
        ret_value = del_attrs_compact_dense_compact(g, gcpl, which);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_modify_default_group_attr
 *
 * Purpose:     Add an attribute then modify the value to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "modify" test.
 *-------------------------------------------------------------------------
 */

static bool
add_modify_default_group_attr(state_t *s, hid_t g, unsigned int which)
{

    bool        ret_value    = false;
    const char *aname_format = "attr-%u";
    ret_value                = add_default_group_attr(s, g, which);
    if (ret_value == true)
        ret_value = modify_attr(s, g, aname_format, which);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    del_ohr_block_attr
 *
 * Purpose:     Add an attribute to force creation of object header
 *              continuation block and remove this attribute to delete
 *              the object header continuation block
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *              The group name is "group-which" and the attribute name
 *              is "attr-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the
 *              "deletion of object header continuation block" test.
 *-------------------------------------------------------------------------
 */

static bool
del_ohr_block_attr(state_t *s, hid_t g, unsigned int which)
{

    bool ret_value = false;
    ret_value      = add_default_group_attr(s, g, which);
    if (ret_value == true)
        ret_value = del_one_attr(s, g, false, true, which);
    return ret_value;
}
/*-------------------------------------------------------------------------
 * Function:    add_group_attribute
 *
 * Purpose:     Check the attribute test pattern and then call the
 *              correponding test function..
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              hid_t gcpl
 *              Object creation property list ID
 *
 *              unsigned int which
 *              The number of iterations for group creation, use to generate
 *              newly created group and attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the write_group() function.
 *-------------------------------------------------------------------------
 */

static bool
add_group_attribute(state_t *s, hid_t g, hid_t gcpl, unsigned int which)
{

    bool ret_value    = false;
    char test_pattern = s->at_pattern;

    switch (test_pattern) {
        case 'c':
            ret_value = add_attrs_compact(s, g, gcpl, which);
            break;
        case 't':
            ret_value = add_attrs_compact_dense(s, g, gcpl, which);
            break;
        case 'C':
            ret_value = add_del_attrs_compact(s, g, gcpl, which);
            break;
        case 'D':
            ret_value = add_del_attrs_compact_dense(s, g, gcpl, which);
            break;
        case 'T':
            ret_value = add_del_attrs_compact_dense_compact(s, g, gcpl, which);
            break;
        case 'M':
            ret_value = add_modify_default_group_attr(s, g, which);
            break;
        case 'v':
            ret_value = add_vlstr_attr(s, g, which);
            break;
        case 'r':
            ret_value = add_del_vlstr_attr(s, g, which);
            break;
        case 'm':
            ret_value = add_modify_vlstr_attr(s, g, which);
            break;
        case 'R':
            ret_value = del_ohr_block_attr(s, g, which);
            break;
        case 'a':
        case 'd':
        case ' ':
        default:
            ret_value = add_default_group_attr(s, g, which);
            break;
    }
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    write_group
 *
 * Purpose:     Create a group and carry out attribute operations(add,delete etc.)
 *              according to the attribute test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static bool
write_group(state_t *s, unsigned int which)
{
    char            name[sizeof("/group-9999999999")];
    hid_t           g       = H5I_INVALID_HID;
    hid_t           dummy_d = H5I_INVALID_HID;
    hid_t           gcpl    = H5I_INVALID_HID;
    bool            result  = true;
    H5G_info_t      group_info;
    struct timespec start_time, end_time;
    double          temp_time;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);

    if (s->old_style_grp)
        gcpl = H5P_DEFAULT;
    else {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);
        if (gcpl < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        /* If we test the dense storage, change the attribute phase. */
        if (s->at_pattern == 'd') {
            if (H5Pset_attr_phase_change(gcpl, 0, 0) < 0) {
                printf("H5Pset_attr_phase_change failed for the dense storage.\n");
                TEST_ERROR;
            }
        }
    }

    if (s->gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {

            fprintf(stderr, "HDclock_gettime failed");

            TEST_ERROR;
        }
    }

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (s->gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {

            fprintf(stderr, "HDclock_gettime failed");

            TEST_ERROR;
        }

        temp_time = TIME_PASSED(start_time, end_time);
        if (temp_time < s->min_gc_time)
            s->min_gc_time = temp_time;
        if (temp_time > s->max_gc_time)
            s->max_gc_time = temp_time;
        s->total_gc_time += temp_time;
    }

    /* We need to create a dummy dataset for the object header continuation block test. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if ((dummy_d = H5Dcreate2(g, "Dataset", H5T_NATIVE_INT, s->one_by_one_sid, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 failed\n");
            TEST_ERROR;
        }
    }
    /* We only need to check the first group */
    if (which == 0) {
        if (H5Gget_info(g, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (s->old_style_grp) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
                printf("Old-styled group test: but the group is not in old-style. \n");
                TEST_ERROR;
            }
            dbgf(2, "Writer: group is created with the old-style.\n");
        }
        else {
            if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
                printf("The created group should NOT be in old-style . \n");
                TEST_ERROR;
            }
            dbgf(2, "Writer: group is created with the new-style.\n");
        }
    }

    /* If coming to an "object header continuation block" test,
     * we need to check if this test behaves as expected. */
    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (false == check_ohr_num_chunk(g, true)) {
            printf("An object header continuation block should NOT be created. \n");
            printf("But it is created.\n");
            TEST_ERROR;
        }
    }

    /* Then carry out the attribute operation. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = add_group_attribute(s, g, gcpl, which);

    if (s->at_pattern == 'a' || s->at_pattern == 'R') {
        if (H5Dclose(dummy_d) < 0) {
            printf("H5Dclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    if (!s->old_style_grp && H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return result;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
        if (s->at_pattern == 'a' || s->at_pattern == 'R')
            H5Dclose(dummy_d);
        if (!s->old_style_grp)
            H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group_id
 *
 * Purpose:     Create a group and return the group ID.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 *              bool dense_to_compact
 *              true if this function is used to test the transition from dense to
 *              compact, false if the test is from compact to dense.
 *
 * Return:      Success:    the group ID
 *              Failure:    -1
 *
 * Note:        Only used by testing the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static hid_t
create_group_id(state_t *s, unsigned int which, bool dense_to_compact)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g    = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    if (gcpl < 0) {
        printf("H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (dense_to_compact) {
        if (H5Pset_link_phase_change(gcpl, 2, 2) < 0) {
            printf("H5Pset_link_phase_change failed for dense to compact.\n");
            TEST_ERROR;
        }
    }
    else {
        if (H5Pset_link_phase_change(gcpl, 1, 1) < 0) {
            printf("H5Pset_attr_phase_change failed for compact to dense.\n");
            TEST_ERROR;
        }
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    /* The storage type should always be compact when a group is created. */
    if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
        printf("New-style group link storage test:. \n");
        printf("    still be compact after group creation. \n");
        TEST_ERROR;
    }

    if (H5Pclose(gcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return g;

error:

    H5E_BEGIN_TRY
    {
        H5Pclose(gcpl);
    }
    H5E_END_TRY;

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    close_group_id
 *
 * Purpose:     Verify is a group is closed successfully.
 *
 * Parameters:  hid_t g
 *              The ID of the group to be closed.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by the link storage transit functions.
 *-------------------------------------------------------------------------
 */

static bool
close_group_id(hid_t g)
{

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    create_group
 *
 * Purpose:     Create a group
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static bool
create_group(state_t *s, unsigned int which)
{

    char       name[sizeof("/group-9999999999")];
    hid_t      g = H5I_INVALID_HID;
    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    esnprintf(name, sizeof(name), "/group-%u", which);
    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        printf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gget_info(g, &group_info) < 0) {
        printf("H5Gget_info failed\n");
        TEST_ERROR;
    }

    if (s->old_style_grp) {
        if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("Old-styled group test: but the group is not in old-style. \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the old-style.\n");
    }
    else {
        if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
            printf("The created group should NOT be in old-style . \n");
            TEST_ERROR;
        }
        dbgf(2, "Writer: group is created with the new-style.\n");
    }

    if (H5Gclose(g) < 0) {
        printf("H5Gclose failed\n");
        TEST_ERROR;
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    delete_one_link
 *
 * Purpose:     Delete a link(either hard/soft) in group operation tests.
 *              according to the group test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              The HDF5 object ID that the deleted link is attached to.
 *
 *              const char *name
 *              The name of the link to be deleted.
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact after link deletion..
 *              >1: link storage should be dense after link deletion.
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is used by delete_groups() and delete_links() functions.
 *-------------------------------------------------------------------------
 */

static bool
delete_one_link(state_t *s, hid_t obj_id, const char *name, short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Ldelete(obj_id, name, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support the indexed storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {

            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {

            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    delete_group
 *
 * Purpose:     Delete a group and carry out group operations(add,delete etc.)
 *              according to the group operation test pattern.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              This is used to generate the group name
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
delete_group(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    bool ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        ret_value = delete_one_link(s, s->file, name, 0, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    move_one_group
 *
 * Purpose:     A helper function used by the move_group operation.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this group is attached to
 *
 *              const char *name
 *              The original group name
 *
 *              const char *newname
 *              The new group name
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the move_group() function.
 *-------------------------------------------------------------------------
 */

static bool
move_one_group(state_t *s, hid_t obj_id, const char *name, const char *newname, unsigned int which)
{

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    if (H5Lmove(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        printf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    move_group
 *
 * Purpose:     Move a group to another group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
move_group(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char new_name[sizeof("/new-group-9999999999")];
    bool ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(new_name, sizeof(new_name), "/new-group-%u", which);
        ret_value = move_one_group(s, s->file, name, new_name, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    insert_one_link
 *
 * Purpose:     A helper function used to attach a link to a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              hid_t obj_id
 *              ID of the object this link is attached to
 *
 *              const char *name
 *              The name of the target object used by creating links
 *
 *              const char *newname
 *              The name of the linked objects
 *
 *              bool is_hard
 *              true if inserting a hard link
 *              false if inserting a soft link
 *
 *              short link_storage
 *              <=0: link storage is ignored.
 *              1: link storage should be compact.
 *              >1: link storage should be dense.

 *              unsigned int which
 *              The number of iterations for group creation
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the insert_links and link storage transit functions.
 *              For link storage, we test at both the writer and the reader.
 *-------------------------------------------------------------------------
*/

static bool
insert_one_link(state_t *s, hid_t obj_id, const char *name, const char *newname, bool is_hard,
                short link_storage, unsigned int which)
{

    H5G_info_t group_info;

    if (which >= s->nsteps) {
        printf("Number of created groups is out of bounds\n");
        TEST_ERROR;
    }

    /* For storage transit and insert_links cases, we
     * create links in different style, just add a little
     * variation of the tests.*/
    if (is_hard) {
        if (link_storage > 0) {
            if (H5Lcreate_hard(s->file, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_hard(obj_id, name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_hard failed\n");
                TEST_ERROR;
            }
        }
    }
    else {
        if (link_storage > 0) {
            if (H5Lcreate_soft("/", obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Lcreate_soft(name, obj_id, newname, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                printf("H5Lcreate_soft failed.\n");
                TEST_ERROR;
            }
        }
    }

    if (link_storage > 0) {

        if (s->old_style_grp) {
            printf("Old style group doesn't support dense or compact storage.\n");
            TEST_ERROR;
        }

        if (H5Gget_info(obj_id, &group_info) < 0) {
            printf("H5Gget_info failed\n");
            TEST_ERROR;
        }

        if (link_storage == 1) {
            if (group_info.storage_type != H5G_STORAGE_TYPE_COMPACT) {
                printf("The group link storage should be compact. \n");
                TEST_ERROR;
            }
        }
        else {
            if (group_info.storage_type != H5G_STORAGE_TYPE_DENSE) {
                printf("The group link storage should be dense. \n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    insert_links
 *
 * Purpose:     create links with a group.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
insert_links(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    bool ret_value = create_group(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = insert_one_link(s, s->file, name, hd_name, true, 0, which);
        if (ret_value == true)
            ret_value = insert_one_link(s, s->file, name, st_name, false, 0, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    delete_links
 *
 * Purpose:     create links with a group and then delete them successfully.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations
 *              used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
delete_links(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    bool ret_value = insert_links(s, which);
    if (ret_value == true) {
        esnprintf(name, sizeof(name), "/group-%u", which);
        esnprintf(hd_name, sizeof(hd_name), "/hd-group-%u", which);
        esnprintf(st_name, sizeof(st_name), "/st-group-%u", which);
        ret_value = delete_one_link(s, s->file, hd_name, 0, which);
        if (ret_value == true)
            ret_value = delete_one_link(s, s->file, st_name, 0, which);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    transit_storage_compact_to_dense
 *
 * Purpose:     Add links so that the link storage transits from
 *              compact to dense.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
transit_storage_compact_to_dense(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("/st-group-9999999999")];

    hid_t g = create_group_id(s, which, false);
    if (g < 0) {
        printf("create_group_id failed\n");
        TEST_ERROR;
    }

    /* First insert a hard link, compact storage. */
    esnprintf(name, sizeof(name), "/group-%u", which);
    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (insert_one_link(s, g, name, hd_name, true, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Then insert a soft link, the storage becomes dense. */
    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (insert_one_link(s, g, name, st_name, false, 2, which) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    if (close_group_id(g) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    transit_storage_dense_to_compact
 *
 * Purpose:     Add or delete links so that the link storage transits from
 *              compact to dense then to compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations used to generate the group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the group_operations() function.
 *-------------------------------------------------------------------------
 */

static bool
transit_storage_dense_to_compact(state_t *s, unsigned int which)
{

    char name[sizeof("/group-9999999999")];
    char hd_name[sizeof("/hd-group-9999999999")];
    char st_name[sizeof("st-group-9999999999")];
    char st2_name[sizeof("st2-group-9999999999")];

    hid_t g = create_group_id(s, which, true);
    if (g < 0) {
        printf("create_group_id failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage is compact. */
    esnprintf(name, sizeof(name), "/group-%u", which);
    esnprintf(hd_name, sizeof(hd_name), "hd-group-%u", which);
    if (insert_one_link(s, g, name, hd_name, true, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage is still compact. */
    esnprintf(st_name, sizeof(st_name), "st-group-%u", which);
    if (insert_one_link(s, g, name, st_name, false, 1, which) == false) {
        printf("insert_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    /* Insert a link, storage becomes dense. */
    esnprintf(st2_name, sizeof(st2_name), "st2-group-%u", which);
    if (insert_one_link(s, g, name, st2_name, false, 2, which) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    /* Delete a link, storage is still dense */
    if (delete_one_link(s, g, st_name, 2, which) == false) {
        printf("delete_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    /* Delete another link, storage becomes compact */
    if (delete_one_link(s, g, st2_name, 1, which) == false) {
        printf("delete_one_link for compact storage failed\n");
        TEST_ERROR;
    }

    if (close_group_id(g) == false) {
        printf("insert_one_link for dense storage failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    group_operations
 *
 * Purpose:     Carry out group and attribute operations(add,delete etc.)
 *              according to the group operation and attribute test patterns.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function. The check of attribute
 *              operations is inside the write_group() function.
 *-------------------------------------------------------------------------
 */
static bool
group_operations(state_t *s, unsigned int which)
{

    bool ret_value    = false;
    char test_pattern = s->grp_op_pattern;

    switch (test_pattern) {
        case 'c':
            ret_value = create_group(s, which);
            break;
        case 'd':
            ret_value = delete_group(s, which);
            break;
        case 'm':
            ret_value = move_group(s, which);
            break;
        case 'i':
            ret_value = insert_links(s, which);
            break;
        case 'D':
            ret_value = delete_links(s, which);
            break;
        case 't':
            ret_value = transit_storage_compact_to_dense(s, which);
            break;
        case 'T':
            ret_value = transit_storage_dense_to_compact(s, which);
            break;
        case ' ':
        default:
            ret_value = write_group(s, which);
            break;
    }
    return ret_value;
}

static unsigned int grp_counter = 0;

/*-------------------------------------------------------------------------
 * Function:    UI_Pow
 *
 * Purpose:     Helper function to obtain the power 'n' of
 *              an unsigned integer 'x'
 *              Similar to pow(x,y) but for an unsigned integer.
 *
 * Parameters:  unsigned int x
 *              unsigned int n
 *
 * Return:      Return an unsigned integer of value of pow(x,n)
 * Note:        If the returned value is > 2^32-1, an overflow
 *              may occur. For our testing purpose, this may never happen.
 *
 *-------------------------------------------------------------------------
 */

static unsigned int
UI_Pow(unsigned int x, unsigned int n)
{
    unsigned int i; /* Variable used in loop grp_counter */
    unsigned int number = 1;

    for (i = 0; i < n; ++i)
        number *= x;

    return (number);
}

/*-------------------------------------------------------------------------
 * Function:    obtain_tree_level_elems
 *
 * Purpose:     Helper function to obtain the maximum number of elements
 *              at one level.
 *
 * Parameters:  unsigned int total_ele
 *              The total number of elements of a tree(excluding the root)
 *
 *              unsigned int level
 *              The number of nested levels
 *              (If every element of the tree is under the root,
 *                  the level is 0.)
 *
 * Return:      Return the maximum number of elements at one level
 *
 * Example:     If the total number of elements is 6 and level is 1,
 *                 the maximum number of elements is 2.The tree is
 *                 a perfectly balanced tree.
 *              Such as:
 *                   0
 *                1     2
 *              3  4  5   6
 *
 *              If the total number of elements is 5 and level is 1,
 *                 the maximum number of elements is still 2. The
 *                 tree is not balanced, there is no element on the
 *                 right-most leaf but the level is still 1.
 *              Such as:
 *                   0
 *                1     2
 *              3  4  5
 *
 *-------------------------------------------------------------------------
 */

static unsigned int
obtain_tree_level_elems(unsigned int total_ele, unsigned int level)
{

    assert(level <= total_ele);
    /* if every element is under the root, just return the total number of elements. */
    if (level == 0)
        return total_ele;
    else {
        unsigned int test_elems_level = 0;
        unsigned     total            = 0;
        unsigned int i                = 1;
        /* Obtain the maximum number of elements for a level with the brutal force way. */
        while (total < total_ele) {
            test_elems_level++;
            total = 0;
            for (i = 1; i <= level + 1; i++)
                total += UI_Pow(test_elems_level, i);
        }
        if (total == total_ele)
            dbgf(2, "Perfectly match: Number of elements per level is %u\n", test_elems_level);
        return test_elems_level;
    }
}

/*-------------------------------------------------------------------------
 * Function:    gen_tree_struct
 *
 * Purpose:     Generate the nested groups
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int level
 *              The number of nested levels +1
 *              (Note: If every element of the tree is under the root,
 *                     the level is 1 in this function.)
 *              unsigned num_elems_per_level
 *              The maximum number of element in a level
 *              hid_t group_id
 *              The ID of the parent group
 *
 * Return:      Success: true
 *              Failure: false
 */
static bool
gen_tree_struct(state_t *s, unsigned int level, unsigned ne_per_level, hid_t pgrp_id)
{

    char            name[sizeof("group-9999999999")];
    unsigned int    i;
    hid_t           grp_id;
    bool            result = true;
    H5G_info_t      group_info;
    struct timespec start_time, end_time;
    double          temp_time;

    if (level > 0 && grp_counter < s->nsteps) {

        for (i = 0; i < ne_per_level; i++) {

            /* For each i a group is created.
               Use grp_counter to generate the group name.
            printf("id: %u,level: %u, index: %u\n",id,level,i);
            */
            esnprintf(name, sizeof(name), "group-%u", grp_counter);
            if (grp_counter == s->nsteps)
                break;

            dbgf(2, "writer in nested group: step %d\n", grp_counter);
            if (s->gperf) {

                if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
                    fprintf(stderr, "HDclock_gettime failed");
                    TEST_ERROR;
                }
            }

            if ((grp_id = H5Gcreate2(pgrp_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                printf("H5Gcreate2 failed\n");
                TEST_ERROR;
            }

            if (s->gperf) {

                if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {

                    fprintf(stderr, "HDclock_gettime failed");

                    TEST_ERROR;
                }

                temp_time = TIME_PASSED(start_time, end_time);
                if (temp_time < s->min_gc_time)
                    s->min_gc_time = temp_time;
                if (temp_time > s->max_gc_time)
                    s->max_gc_time = temp_time;
                s->total_gc_time += temp_time;
            }

            /* Just check the first group information. */
            if (grp_counter == 0) {
                if (H5Gget_info(grp_id, &group_info) < 0) {
                    printf("H5Gget_info failed\n");
                    TEST_ERROR;
                }

                if (s->old_style_grp) {
                    if (group_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) {
                        printf("Old-styled group test: but the group is not in old-style. \n");
                        TEST_ERROR;
                    }
                    dbgf(2, "Writer: group is created with the old-style.\n");
                }
                else {
                    if (group_info.storage_type == H5G_STORAGE_TYPE_SYMBOL_TABLE) {
                        printf("The created group should NOT be in old-style . \n");
                        TEST_ERROR;
                    }
                    dbgf(2, "Writer: group is created with the new-style.\n");
                }
            }

            /* Then carry out the attribute operation. */
            if (s->asteps != 0 && grp_counter % s->asteps == 0)
                result = add_default_group_attr(s, grp_id, grp_counter);

            if (result == false) {
                printf("Cannot create group attributes. \n");
                TEST_ERROR;
            }
            grp_counter++;

            /* Generate groups in the next level */
            result = gen_tree_struct(s, level - 1, ne_per_level, grp_id);
            if (result == false) {
                printf("Cannot create nested groups. \n");
                TEST_ERROR;
            }

            /*  close the group ID. No problem. */
            if (H5Gclose(grp_id) < 0) {
                printf("H5Gclose failed. \n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:

    H5E_BEGIN_TRY
    {
        H5Gclose(grp_id);
    }
    H5E_END_TRY;

    return false;
}

int
main(int argc, char **argv)
{
    hid_t                 fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned              step;
    bool                  writer = false;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    bool                  wg_ret = false;
    struct timespec       start_time, end_time;
    unsigned int          num_elems_per_level;

    if (!state_init(&s, argc, argv)) {
        printf("state_init failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_gperf_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_gperf_writer") == 0)
        writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_gperf_reader") == 0)
        writer = false;
    else {
        printf("unknown personality, expected vfd_swmr_gperf_{reader,writer}\n");
        TEST_ERROR;
    }

    if (writer == false) {
        printf("Reader is skipped for the performance tests.\n");
        return EXIT_SUCCESS;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, s.tick_len, s.max_lag, writer, FALSE, 128, "./group-shadow");

    /* If old-style option is chosen, use the earliest file format(H5F_LIBVER_EARLIEST)
     * as the second parameter of H5Pset_libver_bound() that is called by
     * vfd_swmr_create_fapl. Otherwise, the latest file format(H5F_LIBVER_LATEST)
     * should be used as the second parameter of H5Pset_libver_bound().
     * Also pass the use_vfd_swmr, only_meta_page, page buffer size, config to vfd_swmr_create_fapl().*/
    if ((fapl = vfd_swmr_create_fapl(!s.old_style_grp, s.use_vfd_swmr, true, s.pbs, &config)) < 0) {
        printf("vfd_swmr_create_fapl failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s.ps)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (s.nglevels > 0) {
        if (s.grp_op_pattern != ' ' || s.at_pattern != ' ') {
            printf("For nested group creation test, only the default option is supported.\n");
            printf("Please re-run the tests with the appopriate option.\n");
            TEST_ERROR;
        }
    }

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
    }

    s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        s.fo_total_time = TIME_PASSED(start_time, end_time);
    }

    if (s.file < 0) {
        printf("H5Fcreate failed\n");
        TEST_ERROR;
    }

    /* If generating nested groups, calculate the maximum number of
          elements per level.  */
    if (s.nglevels > 0)
        num_elems_per_level = obtain_tree_level_elems(s.nsteps, s.nglevels);

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
    }

    /* If generating nested groups */
    if (s.nglevels > 0) {

        /* for the recursive call, the groups under the root is treated as one level */
        wg_ret = gen_tree_struct(&s, s.nglevels + 1, num_elems_per_level, s.file);
        if (wg_ret == false) {
            printf("write nested group failed at group counter  %u\n", grp_counter);
            TEST_ERROR;
        }
    }
    else {
        for (step = 0; step < s.nsteps; step++) {

            dbgf(2, "writer: step %d\n", step);
            wg_ret = group_operations(&s, step);
            if (wg_ret == false) {
                printf("write_group failed at step %d\n", step);
                TEST_ERROR;
            }
        }
    }

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {

            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        s.total_time   = TIME_PASSED(start_time, end_time);
        s.mean_time    = s.total_time / s.nsteps;
        s.mean_gc_time = s.total_gc_time / s.nsteps;
    }

    if (H5Pclose(fapl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s.one_by_one_sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {

            fprintf(stderr, "HDclock_gettime failed");

            TEST_ERROR;
        }
    }

    if (H5Fclose(s.file) < 0) {
        printf("H5Fclose failed\n");
        TEST_ERROR;
    }

    if (s.gperf) {

        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {

            fprintf(stderr, "HDclock_gettime failed");

            TEST_ERROR;
        }

        s.fc_total_time = TIME_PASSED(start_time, end_time);
    }

    /* Performance statistics summary */
    if (s.gperf) {

        if (verbosity != 0) {

            fprintf(stdout, "\nPerformance Test Configuration: ");
            if (s.use_vfd_swmr)
                fprintf(stdout, " Using VFD SWMR \n");
            else
                fprintf(stdout, " Not using VFD SWMR \n");

            if (s.old_style_grp)
                fprintf(stdout, " Groups: Created via the earlist file format(old-style) \n");
            else
                fprintf(stdout, " Groups: Created via the latest file format(new-style) \n");

            fprintf(stdout, "\n");

            fprintf(stdout, "The length of a tick              = %u\n", s.tick_len);
            fprintf(stdout, "The maximum expected log(in ticks)= %u\n", s.max_lag);
            fprintf(stdout, "The page size(in bytes)           = %u\n", s.ps);
            fprintf(stdout, "The page buffer size(in bytes)    = %u\n", s.pbs);
            fprintf(stdout, "\n");
            fprintf(stdout, "Number of groups                  = %u\n", s.nsteps);
            fprintf(stdout, "Group Nested levels               = %u\n", s.nglevels);
            fprintf(stdout, "Number of attributes              = %u\n", s.num_attrs);
            fprintf(stdout, "Number of element per attribute   = 1\n");
            if (s.vlstr_test)
                fprintf(stdout, "Attribute datatype is variable length string. \n");
            else if (s.filetype == H5T_STD_U32BE)
                fprintf(stdout, "Attribute datatype is big-endian unsigned 32-bit integer.\n");
            else
                fprintf(stdout, "Attribute datatype is native unsigned 32-bit integer.\n");

            fprintf(stdout, "\n");
            fprintf(stdout,
                    "(If the nested level is 0, all the groups are created directly under the root.)\n\n");
            fprintf(stdout, "group creation maximum time                       =%lf\n", s.max_gc_time);
            fprintf(stdout, "group creation minimum time                       =%lf\n", s.min_gc_time);
        }

        fprintf(stdout, "group creation total time                           = %lf\n", s.total_gc_time);
        fprintf(stdout, "group creation mean time(per group)                 = %lf\n", s.mean_gc_time);
        fprintf(stdout, "group creation and attributes generation total time = %lf\n", s.total_time);
        fprintf(stdout, "group creation and attributes generation mean time(per group) = %lf\n", s.mean_time);
        fprintf(stdout, "H5Fcreate time = %lf\n", s.fo_total_time);
        fprintf(stdout, "H5Fclose time  = %lf\n", s.fc_total_time);
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Sclose(s.one_by_one_sid);
        H5Fclose(s.file);
    }
    H5E_END_TRY;

    return EXIT_FAILURE;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
