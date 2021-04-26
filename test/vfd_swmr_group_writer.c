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

#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define READER_WAIT_TICKS	3
#define VS_ATTR_NAME_LEN       21

typedef struct {
   hid_t file, filetype, one_by_one_sid;
   char filename[PATH_MAX];
   char progname[PATH_MAX];
   unsigned int asteps;
   unsigned int csteps;
   unsigned int nsteps;
   unsigned int update_interval;
   bool use_vfd_swmr;
   bool use_named_pipes;
   char at_pattern;
   bool attr_test;
   uint32_t max_lag;
   uint32_t tick_len;
   int np_fd_w_to_r;
   int np_fd_r_to_w;
   int np_notify;
   int np_verify;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .asteps = 10, .csteps = 10, .nsteps = 100, .update_interval = READER_WAIT_TICKS,   \
        .use_vfd_swmr = true,                                                                                \
        .use_named_pipes = true                                                                              \
        , .at_pattern = ' '                 \
        , .attr_test = false                \
        , .tick_len  = 4                    \
        , .max_lag   = 7                    \
        , .np_fd_w_to_r = -1                \
        , .np_fd_r_to_w = -1                \
        , .np_notify = 0                    \
        , .np_verify = 0   }


//TODO: add at_pattern description
static void
usage(const char *progname)
{
    fprintf(stderr, "usage: %s [-S] [-a steps] [-b] [-c]\n"
                "    [-n iterations] [-N] [-q] [-u numb_ticks] [-A at_pattern]\n"
        "\n"
        "-S:             do not use VFD SWMR\n"
        "-a steps:       `steps` between adding attributes\n"
        "-b:             write data in big-endian byte order\n"
        "-c steps:       `steps` between communication between the writer and reader\n"
        "-n ngroups:     the number of groups\n"
        "-N:             do not use named pipes, \n"
        "                mainly for running the writer and reader seperately\n"
        "-u numb_ticks:  `numb_ticks` for the reader to wait before verification\n"
        "-A at_pattern:  `at_pattern' for different attribute tests\n"
        "              The value of `at_pattern` is one of the following:\n"
        "              `compact`              - Attributes added in compact storage\n"
        "              `dense`                - An attribute added in dense storage\n"
        "              `compact-to-dense`     - Attributes added first in compact\n"
        "                                       then in dense storage\n"
        "              `compact-del`          - Attributes added and then one\n"
        "                                       attribute deleted, in compact \n"
        "              `dense-del`            - Attributes added until the storage\n"
        "                                       is dense then an attribute deleted\n"
        "                                       the storge still in dense\n"
        "              `dense-del-to-compact` - Attributes added until the storage\n" 
        "                                       is dense, then several attributes \n"
        "                                       deleted, the storage changed to\n"
        "                                       compact\n"
        "              `modify`               - An attribute added then modified\n"
        "              `vstr`                 - A VL string attribute added\n"
        "              `remove-vstr`          - A VL string attribute added then\n"
        "                                       deleted\n"
        "              `modify-vstr`          - A VL string attribute added then \n"
        "                                       modified \n"
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
    const hsize_t dims = 1;
    char          *tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        H5_FAILED(); AT();
        printf("H5_basename failed\n");
        goto error;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile)
        HDfree(tfile);

    while ((ch = getopt(argc, argv, "Sa:bc:n:Nqu:A:")) != -1) {
        switch (ch) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'a':
            case 'c':
            case 'n':
            case 'u':
                errno = 0;
                tmp = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    H5_FAILED(); AT();
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    goto error;
                } else if (errno != 0) {
                    H5_FAILED(); AT();
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    goto error;
                } else if (tmp > UINT_MAX) {
                    H5_FAILED(); AT();
                    printf("`-%c` argument `%lu` too large\n", ch, tmp);
                    goto error;
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->csteps = (unsigned)tmp;
                else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'u')
                    s->update_interval = (unsigned)tmp;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'N':
                s->use_named_pipes = false;
                break;
            case 'A':
                if (strcmp(optarg, "compact") == 0)
                    s->at_pattern = 'c';
                else if (strcmp(optarg, "dense") == 0)
                    s->at_pattern = 'd';
                else if (strcmp(optarg, "compact-to-dense") == 0) 
                    s->at_pattern = 't';
                else if (strcmp(optarg, "compact-del") == 0) 
                    s->at_pattern = 'C';
                else if (strcmp(optarg, "dense-del") == 0) 
                    s->at_pattern = 'D';
                else if (strcmp(optarg, "dense-del-to-compact") == 0) 
                    s->at_pattern = 'T';
                else if (strcmp(optarg, "modify") == 0) 
                    s->at_pattern = 'M';
                else if (strcmp(optarg,"vstr") ==0)
                    s->at_pattern = 'v';
                else if (strcmp(optarg, "remove-vstr") == 0) 
                    s->at_pattern = 'r';
                else if (strcmp(optarg, "modify-vstr") == 0) 
                    s->at_pattern = 'm';
                else {
                    H5_FAILED(); AT();
                    printf("Invalid -A argument \"%s\"", optarg);
                    goto error;
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

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        H5_FAILED(); AT();
        printf("H5Screate_simple failed\n");
        goto error;
    }

    if( s->csteps < 1 || s->csteps > s->nsteps) {
        H5_FAILED(); AT();
        printf("communication interval is out of bounds\n");
        goto error;
    }

    if( s->asteps < 1 || s->asteps > s->nsteps) {
        H5_FAILED(); AT();
        printf("attribute interval is out of bounds\n");
        goto error;
    }

    if (argc > 0) {
        H5_FAILED(); AT();
        printf("unexpected command-line arguments\n");
        goto error;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);
    return false;
}

/* Named Pipe Subroutine: np_wr_send_receive
 * Description:
 *   The writer sends a message to the reader,  
 *   then waits for max_lag ticks,
 *   then checks the returned message from the reader. 
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above. 
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent 
 *         by the reader.
 */ 
static bool np_wr_send_receive(state_t *s) {

    unsigned int i;
    /* Bump up the value of notify to notice the reader to start to read */
    s->np_notify++;
    if (HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED(); AT();
        printf("HDwrite failed\n");
        goto error;
    }

    /* During the wait, writer makes repeated HDF5 API calls
       * to trigger EOT at approximately the correct time */
    for(i = 0; i < s->max_lag + 1; i++) {
        decisleep(s->tick_len);
        H5E_BEGIN_TRY {
            H5Aexists(s->file, "nonexistent");
        } H5E_END_TRY;
    }

    /* Receive the same value from the reader and verify it before
     * going to the next step */
    (s->np_verify)++;
    if (HDread(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0){
        H5_FAILED(); AT();
        printf("HDread failed\n");
        goto error;
    }

    if (s->np_notify == -1) {
        H5_FAILED(); AT();
        printf("reader failed to verify group or attribute operation.\n");
        goto error;
    }

    if (s->np_notify != s->np_verify) {
        H5_FAILED(); AT();
        printf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        goto error;
    }

    return true;

error:
    return false;

}

/* Named Pipe Subroutine: np_rd_receive
 * Description:
 *   The reader receives a message from the writer,  
 *   then checks if the notification number from
 *   the writer is expected.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above. 
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent 
 *         by the writer.
 */ 
static bool np_rd_receive(state_t *s) {

    /* The writer should have bumped up the value of notify.
     * Do the same with verify and confirm it */
    s->np_verify++;

    /* Receive the notify that the writer bumped up the value */
    if (HDread(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED(); AT();
        printf("HDread failed\n");
        goto error;
    }

    if (s->np_notify == -1) {
        H5_FAILED(); AT();
        printf("writer failed to create group or carry out an attribute operation.\n");
        goto error;
    }

    if (s->np_notify != s->np_verify) {
        H5_FAILED(); AT();
        printf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        goto error;
    }
 
    return true;

error:
    return false;
}

/* Named Pipe Subroutine: np_rd_send
 * Description:
 *   The reader sends an acknowledgement to the writer  
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message. 
 */ 
static bool np_rd_send(state_t *s) {

    if (HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED(); AT();
        printf("HDwrite failed\n");
        return false;
    }
    else
        return true;
}

/* Named Pipe Subroutine: np_send_error
 * Description:
 *   An error (notification number is 1) message is sent 
 *   either from the reader or the writer.
 *   A boolean input parameter is used to choose
 *   either reader or writer. 
 * Return:
 *     None
 */
static void np_send_error(state_t *s,bool writer) {
    s->np_notify = -1;
    if(writer) 
        HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int));
    else 
        HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int));
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
add_attr(state_t *s, 
         hid_t oid,
         unsigned int which,
         unsigned num_attrs,
         const char*aname_fmt,
         unsigned int g_which) {

    char attrname[VS_ATTR_NAME_LEN];
    unsigned u;
    unsigned attr_value;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;
    hid_t atype = s->filetype;
    hid_t sid = s->one_by_one_sid;

    /* Need to obtain native datatype for H5Aread */
    if((amtype = H5Tget_native_type(atype,H5T_DIR_ASCEND)) <0) {
        H5_FAILED(); AT();
        printf("H5Tget_native_type failed\n");
        goto error;
    }

    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        /* Construct attribute name like attr-0-0 */
        HDsprintf(attrname, aname_fmt, which,u);
        if((aid = H5Acreate2(oid, attrname, atype, sid, H5P_DEFAULT, 
            H5P_DEFAULT)) < 0) { 
            H5_FAILED(); AT();
            printf("H5Acreate2 failed\n");
            goto error;
        }

        attr_value = u+which;
#if 0
        // Just for debugging to check if error handling works.
        attr_value = u+which+1;
#endif

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, g_which, u+which);

        /* Write data into the attribute */
        if (H5Awrite(aid, amtype, &attr_value) < 0) {
            H5_FAILED(); AT();
            printf("H5Awrite failed\n");
            goto error;
        }

        /* Close attribute */
        if(H5Aclose(aid) < 0) {
            H5_FAILED(); AT();
            printf("H5Aclose failed\n");
            goto error;
        }

        /* Writer sends a message to reader: an attribute is successfully generated. 
           then wait for the reader to verify and send an acknowledgement message back.*/
        if (s->use_named_pipes && s->attr_test == true) {
            dbgf(2, "writer: write attr - ready to send/receive message: %d\n", s->np_notify+1);
            if(np_wr_send_receive(s) == false) {
                H5_FAILED(); AT();
                dbgf(2, "writer: write attr - verification failed.\n");
                /* Note: This is (mostly) because the verification failure message
                 *       from the reader. So don't send the error message back to
                 *       the reader. Just stop the test. */
                goto error2;
            }
        }
       
    } /* end for */

    if(H5Tclose(amtype) < 0) {
        H5_FAILED(); AT();
        goto error;
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,true);

error2: 
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(amtype);
    } H5E_END_TRY;

    return false;

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
 *              It is also used by the group-only test.
 *-------------------------------------------------------------------------
*/ 

static bool 
add_default_group_attr(state_t *s, hid_t g, unsigned int which) {

    const char* aname_format ="attr-%u";

    /* Note: Since we only add one attribute, the parameter for
    *        the number of attributes is 1. */
    return add_attr(s,g,which,1,aname_format,which);

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
add_vlstr_attr(state_t*s, hid_t g, unsigned int which) {

    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;
    hid_t sid = s->one_by_one_sid;

    /* Allocate buffer for the VL string value */
    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for VL string failed.\n");
        goto error;
    }

    /* Assign the VL string value and the attribute name.. */
    HDsprintf(astr_val,"%u",which);
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED(); AT();
        printf("Cannot create variable length datatype.\n");
        goto error;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        H5_FAILED(); AT();
        printf("Cannot set variable length datatype.\n");
        goto error;
    }

    /* Generate the VL string attribute.*/
    if ((aid = H5Acreate2(g, name, atype, sid, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Acreate2 failed.\n");
        goto error;
    }

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        H5_FAILED(); AT();
        printf("H5Awrite failed.\n");
        goto error;
    }

    if (H5Tclose(atype) < 0) {
        H5_FAILED(); AT();
        printf("H5Tclose() failed\n");
        goto error;
    }
    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose() failed\n");
        goto error;
    }

    HDfree(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated. 
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: write attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,true);
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(atype);
    } H5E_END_TRY;

    if(astr_val)
        HDfree(astr_val);

error2: 
    return false;
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
 *              bool is_vl
 *              if the deleted attribute is a VL string
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
del_one_attr(state_t *s, hid_t obj_id,bool is_dense,bool is_vl,unsigned int which) {

    char attrname[VS_ATTR_NAME_LEN];

    /*attribute name template for the dense storage related deletion operation */
    const char* aname_format_d = "attr-d-%u-%u";

    /*attribute name template used for general attribute deletion operation */
    const char* aname_format = "attr-%u-%u";

    /*attribute name template used for VL string attribute deletion operation */
    const char* aname_format_vl="attr-%u";
    
    dbgf(2, "writer: coming to delete the attribute.\n");

    /* Construct the attribute name */
    if(is_dense == true)  
       HDsprintf(attrname, aname_format_d, which,0);
    else if(is_vl == true)
       HDsprintf(attrname, aname_format_vl, which,0);
    else
       HDsprintf(attrname, aname_format, which,0);
    
    /* Delete the attribute */
    if(H5Adelete(obj_id, attrname) <0) {
        H5_FAILED(); AT();
        printf("H5Adelete() failed\n");
        goto error;
    }

    /* Writer sends a message to reader: an attribute is successfully generated. 
       then wait for the reader to verify and send an acknowledgement message back. */
    if(s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);

error2:
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
add_del_vlstr_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;

    /* Add a VL string attribute then delete it. */
    ret_value = add_vlstr_attr(s,g,which);
    if(ret_value == true) 
        ret_value = del_one_attr(s,g,false,true,which);

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
modify_attr(state_t *s, hid_t g, const char* aname_fmt,unsigned int which) {

    char attrname[VS_ATTR_NAME_LEN];
    hid_t aid = H5I_INVALID_HID;   
    hid_t amtype = H5I_INVALID_HID;
    unsigned int modify_value;

    HDsprintf(attrname,aname_fmt,which,0);
    if((aid = H5Aopen(g,attrname,H5P_DEFAULT))<0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

    if((amtype = H5Tget_native_type(s->filetype,H5T_DIR_ASCEND))<0) {
        H5_FAILED(); AT();
        printf("H5Tget_native_type failed\n");
        goto error;
    }
 
    /* Make a large number to verify the change easily */
    modify_value = which+10000; 

    if (H5Awrite(aid,amtype,&modify_value) <0) {
        H5_FAILED(); AT();
        printf("H5Awrite failed\n");
        goto error;
    }
    if (H5Tclose(amtype) < 0) {
        H5_FAILED(); AT();
        goto error;
    }
    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        goto error;
    }

    /* Writer sends a message to reader: an attribute is successfully modified. 
           then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: modify attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }

    return true;
error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(aid);
    } H5E_END_TRY;

error2:
    return false;

}

/*-------------------------------------------------------------------------
 * Function:    modify_vlstr_attr
 *
 * Purpose:     Modify the value of an VL string attribute in a group.
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
 *              newly created group name. The group name is "group-which".
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 *-------------------------------------------------------------------------
*/ 



static bool 
modify_vlstr_attr(state_t*s,hid_t g, unsigned int which) {

    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;

    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for VL string failed.\n");
        goto error;
    }

    /* Change the VL string value and create the attribute name. */
    HDsprintf(astr_val,"%u%c",which,'A');
    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    /* Create a datatype to refer to. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED(); AT();
        printf("Cannot create variable length datatype.\n");
        goto error;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        H5_FAILED(); AT();
        printf("Cannot set variable length datatype.\n");
        goto error;
    }

    /* Open this attribute. */
    if ((aid = H5Aopen(g, name, H5P_DEFAULT))<0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed.\n");
        goto error;
    }

    dbgf(1, "The modified VL string value  is  %s \n", astr_val);

    if (H5Awrite(aid, atype, &astr_val) < 0) {
        H5_FAILED(); AT();
        printf("H5Awrite failed.\n");
        goto error;
    }

    if (H5Tclose(atype) < 0) {
        H5_FAILED(); AT();
        printf("H5Tclose() failed\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose() failed\n");
        goto error;
    }

    HDfree(astr_val);

    /* Writer sends a message to reader: a VL string attribute is successfully generated. 
       then wait for the reader to verify and send an acknowledgement message back. */
    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: modify vl attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: write attr - verification failed.\n");
            goto error2;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(atype);
    } H5E_END_TRY;

    if(astr_val)
        HDfree(astr_val);

    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,true);

error2:
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
add_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;
    ret_value = add_vlstr_attr(s,g,which);
    if (true == ret_value) 
        ret_value = modify_vlstr_attr(s,g,which);

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
add_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    unsigned max_compact = 0;
    unsigned min_dense = 0;
    const char* aname_format="attr-%u-%u";
    
    /* Obtain the maximal number of attributes to be stored in compact
     * storage and the minimal number of attributes to be stored in
     * dense storage. */
    if(H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense)<0) {
        H5_FAILED(); AT();
        printf("H5Pget_attr_phase_change() failed\n");
        goto error;
    }
    
    /* Add max_compact attributes, these attributes are stored in
     * compact storage. */
    return add_attr(s,g,which,max_compact,aname_format,which);

error:
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);
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
add_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    unsigned max_compact = 0;
    unsigned min_dense = 0;
    const char* aname_format="attr-d-%u-%u";
    bool ret_value = false;

    if(H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        H5_FAILED(); AT();
        printf("H5Pget_attr_phase_change failed\n");
        goto error;
    }

    /* Add attributes, until just before converting to dense storage */
    ret_value = add_attrs_compact(s, g, gcpl, which);

    /* Add another attribute, the storage becomes dense. */
    if(ret_value == true) 
        ret_value = add_attr(s,g,which+max_compact,1,aname_format,which);

    return ret_value;

error:
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);
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
 * Note:        This is an internal function used by the 
 *              "dense-del-to-compact" test. 
 *              For attribute compact/dense storage, check the reference
 *              manual of H5Pget_attr_phase_change.
 *-------------------------------------------------------------------------
*/ 

static bool
del_attrs_compact_dense_compact(state_t *s, 
                                hid_t obj_id,
                                hid_t gcpl,
                                unsigned int which) {
    
    unsigned max_compact = 0;
    unsigned min_dense = 0;
    unsigned u = 0;

    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format="attr-%u-%u";
    const char* adname_format="attr-d-%u-%u";

    /* Obtain the maximal number of attributes to be stored in compact
     * storage and the minimal number of attributes to be stored in
     * dense storage. */
    if (H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        H5_FAILED(); AT();
        printf("H5Pget_attr_phase_change failed\n");
        goto error;
    }
    u= max_compact +1;

#if 0
    if(max_compact < min_dense) 
        printf("Minimum number of attributes stored in dense storage should be less than maximum number of attributes stored in compact storage.\n");
#endif

    // delete a number of attributes so that the attribute storage just becomes dense.
    for(u--;u>=(min_dense-1);u--) {
        HDsprintf(attrname, aname_format, which,max_compact-u);
        if (H5Adelete(obj_id,attrname) < 0) {
            H5_FAILED(); AT();
            printf("H5Adelete failed\n");
            goto error;
        }

        /* For each attribute deletion, we want to ensure the verification
         * from the reader. 
         * So writer sends a message to reader: an attribute is successfully deleted. 
           then wait for reader to verify and send an acknowledgement message back. */
        if(s->use_named_pipes && s->attr_test == true) {
            dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify+1);
            if(np_wr_send_receive(s) == false) {
                H5_FAILED(); AT();
                dbgf(2, "writer: delete attr - verification failed.\n");
                goto error2;
            }
        }
    }

    /* The writer deletes another attribute, the storage is
     * still dense. However, the attribute to be deleted
     * doesn't follow the previous for loop. It may be 
     * in different location in the object header. Just add
     * a litter variation to check if this operation is successful. 
     * The attribute name to be deleted is attr-max_compact+which-0
     */
     
    HDsprintf(attrname,adname_format,max_compact+which,0);
    if (H5Adelete(obj_id,attrname) < 0) {
            H5_FAILED(); AT();
            printf("H5Adelete failed\n");
            goto error;
    }
    /* Again we need to notify the reader via Named pipe. */
    if(s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    /* The following comments are left here in case in the future we want to
     * use HDF5 function to verify the attribute storage */
#if 0   
    // May H5Oget_info3 -- obtain the number of attributes. 
    //Check the number of attributes >=min_dense. 
    //We may use the internal function  
    //is_dense = H5O__is_attr_dense_test(dataset) to check if it is dense in the future. 
    //
#endif

    return true;

error: 
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);

error2:
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
add_del_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    bool ret_value = false;
    ret_value = add_attrs_compact(s, g, gcpl, which);
    if(ret_value == true) {
        dbgf(2, "writer: before deleting the attribute.\n");
        ret_value = del_one_attr(s,g,false,false,which);
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
add_del_attrs_compact_dense(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    bool ret_value = false;
    unsigned max_compact = 0;
    unsigned min_dense = 0;

    if( H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense) < 0) {
        H5_FAILED(); AT();
        printf("H5Pget_attr_phase_change failed\n");
        goto error;
    }

    ret_value = add_attrs_compact_dense(s,g,gcpl,which);
    if(ret_value == true) 
        ret_value = del_one_attr(s,g,true,false,which+max_compact);

    return ret_value;

error:
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);
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
add_del_attrs_compact_dense_compact(state_t *s, 
                                    hid_t g, 
                                    hid_t gcpl, 
                                    unsigned int which) {

    bool ret_value = false;
    ret_value = add_attrs_compact_dense(s,g,gcpl,which);
    if(ret_value == true) 
        ret_value = del_attrs_compact_dense_compact(s,g,gcpl,which);

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
 * Note:        This function is used for the "modify" storage test.
 *-------------------------------------------------------------------------
*/ 


static bool
add_modify_default_group_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;
    const char* aname_format ="attr-%u";
    ret_value = add_default_group_attr(s,g,which);
    if(ret_value == true)
        ret_value = modify_attr(s,g,aname_format,which);
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
add_group_attribute(state_t *s, hid_t g,  hid_t gcpl, unsigned int which)
{

    bool ret_value = false;
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
            ret_value = add_modify_default_group_attr(s, g,  which);
            break;
        case 'v':
            ret_value = add_vlstr_attr(s,g,  which);
            break;
        case 'r':
            ret_value = add_del_vlstr_attr(s, g, which);
            break;
        case 'm':
            ret_value = add_modify_vlstr_attr(s,g,  which);
            break;
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
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    hid_t gcpl = H5I_INVALID_HID;
    bool result = true;

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("Number of created groups is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    if(gcpl <0) {
        H5_FAILED(); AT();
        printf("H5Pcreate failed\n");
        goto error;
    }

    /* If we test the dense storage, change the attribute phase. */
    if(s->at_pattern =='d') {
        if(H5Pset_attr_phase_change(gcpl, 0, 0) <0) {
            H5_FAILED(); AT();
            printf("H5Pset_attr_phase_change failed for the dense storage.\n");
            goto error;
        }
    }

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, 
                        H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gcreate2 failed\n");
        goto error;
    }

    /* If an attribute test is turned on and named pipes are used,
     * the writer should send and receive messages after the group creation.
     * This will distinguish an attribute operation error from an
     * group creation error. 
     * Writer sends a message to reader: an attribute is successfully generated. 
     * then wait for the reader to verify and send an acknowledgement message back.*/
    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            /* Note: This is (mostly) because the verification failure message
             *       from the reader. So don't send the error message back to
             *       the reader. Just stop the test. */
            goto error2;
        }
    }
    
    /* Then carry out the attribute operation. */
    if (s->asteps != 0 && which % s->asteps == 0) 
        result = add_group_attribute(s, g, gcpl,which);

    if (H5Gclose(g) < 0) {
        H5_FAILED(); AT();
        printf("H5Gclose failed\n");
        goto error;
    }

    if(H5Pclose(gcpl) <0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed\n");
        goto error;
    }

    return result;

error:
    /* Writer needs to send an error message to the reader to stop the test*/
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);

error2: 

    H5E_BEGIN_TRY {
        H5Gclose(g);
        H5Pclose(gcpl);
    } H5E_END_TRY;

    return false;

}

/*-------------------------------------------------------------------------
 * Function:    vrfy_attr
 *
 * Purpose:     Verify is a group attribute value is as expected.
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
 *              const char*aname
 *              The attribute name 
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
vrfy_attr(state_t *s, 
          hid_t g, 
          unsigned int which,  
          const char* aname, 
          unsigned int g_which) {

    unsigned int read_which;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if receiving an error 
     * message.
     */ 
    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            /* Since receiving the error message from the writer,
             * just stop the test. */
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }

    /* Go ahead to read the attribute. */
    dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, g_which,
        which);

    if ((amtype = H5Tget_native_type(s->filetype,H5T_DIR_ASCEND)) <0) {
        H5_FAILED(); AT();
        printf("H5Tget_native_type failed\n");
        goto error;
    }

    if ((aid = H5Aopen(g, aname, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

    if (H5Aread(aid, amtype, &read_which) < 0) {
        H5_FAILED(); AT();
        printf("H5Aread failed\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose failed\n");
        goto error;
    }

    if(read_which != which) {
        H5_FAILED(); AT();
        dbgf(2, "reader: the add_attribute verfication failed,expected value is  %d\n", which);
        dbgf(2, "reader: the add_attribute verfication failed, the value is %d\n", read_which);
        printf("The add_attribute verification failed\n");
        goto error;
    }

    /* If the read value is expected, send back an OK message to the writer. */
    if(s->use_named_pipes && s->attr_test == true) {
        if(np_rd_send(s)==false) 
            goto error;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }
    return true;

error:
    H5E_BEGIN_TRY {
        H5Tclose(amtype);
        H5Aclose(aid);
    } H5E_END_TRY;

    /* Send back an error message to the writer so that the writer can stop. */
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);
error2:
    return false;

}

/*-------------------------------------------------------------------------
 * Function:    verify_default_group_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of a
 *              group attribute corrected by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The expected attribute value. It is also used to construct the
 *              group name.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "dense" storage test.
 *              It is also used by the group-only test.
 *-------------------------------------------------------------------------
*/ 

static bool
verify_default_group_attr(state_t*s,hid_t g, unsigned int which)
{
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format = "attr-%u";
    HDsprintf(attrname, aname_format, which);
    return vrfy_attr(s,g,which,attrname,which);

}

/*-------------------------------------------------------------------------
 * Function:    verify_modify_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of 
 *              an attribute in a group, first the original value then
 *              the modified value.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              The expected attribute value. It is also used to construct the
 *              group name. The modified attribute value can be derived from
 *              the expected attribute value. 
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used for the "modified" test.
 *-------------------------------------------------------------------------
*/ 

static bool
verify_modify_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret = false;
    const char* aname_fmt ="attr-%u";
    unsigned int read_which;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;
    char attrname[VS_ATTR_NAME_LEN];

    /* First verify the original attribute value */
    ret = verify_default_group_attr(s,g,which);
 
    /* Then the modified value */
    if(ret == true) {

        /* The reader receives a message from the writer.Then sleep
         * for a few ticks or stop the test if receiving an error 
         * message.
         */ 
        if(s->use_named_pipes && true == s->attr_test) {
            if(false == np_rd_receive(s)) {
                H5_FAILED(); AT();
                goto error2;
            }
            decisleep(s->tick_len * s->update_interval);
            dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
        }

        /* Go ahead to read the attribute. */
        esnprintf(attrname, sizeof(attrname), aname_fmt, which);
        if ((amtype = H5Tget_native_type(s->filetype,H5T_DIR_ASCEND)) < 0) {
            H5_FAILED(); AT();
            printf("H5Tget_native_type failed\n");
            goto error;
        }

        if ((aid = H5Aopen(g, attrname, H5P_DEFAULT)) < 0) {
            H5_FAILED(); AT();
            printf("H5Aopen failed\n");
            goto error;
        }

        if (H5Aread(aid, amtype, &read_which) < 0) {
            H5_FAILED(); AT();
            printf("H5Aread failed\n");
            goto error;
        }

        if(H5Tclose(amtype) <0) {
            H5_FAILED(); AT();
            printf("H5Tclose failed.\n");
           goto error;
        }
 
        if (H5Aclose(aid) < 0) {
            H5_FAILED(); AT();
            printf("H5Aclose failed\n");
            goto error;
        }

        /* verify the modified value */
        if(read_which != (which+10000)) {
            H5_FAILED(); AT();
            dbgf(2, "reader: the modified_attr() expected value is  %d\n", (-1)*(int)which);
            dbgf(2, "reader: the modified_attr() actual value is %d\n", read_which);
            printf("The modify_attribute verification failed.\n");
            goto error;
        }

        /* The reader sends an OK message back to the writer. */
        if(s->use_named_pipes && s->attr_test == true) {
            if(np_rd_send(s)==false) 
                goto error2;
            dbgf(2, "reader: modify_attr finish sending back the message: %d\n", s->np_notify);
        }
        return true;
        
    }
    return false;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(amtype);
    } H5E_END_TRY;

    /* The reader needs to send an error message back to the writer to stop the test.*/
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);

error2:

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group_vlstr_attr
 *
 * Purpose:     Check if the reader can retrieve the correct value of  
 *              a variable length string attribute created by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value. It is also used 
 *              to construct the group name.
 *
 *              bool vrfy_mod
 *              true if this function is used for the modified VL string test.
 *              false if this function is just used for the VL string test.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is an internal function used by 
 *              both the "vlstr" and the "modify-vstr" tests.
 *-------------------------------------------------------------------------
*/ 


static bool
verify_group_vlstr_attr(state_t*s, hid_t g, unsigned int which, bool vrfy_mod)
{
    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char name[VS_ATTR_NAME_LEN];

    char *astr_val_exp;
    char * astr_val;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message 
     * is an error message.
     */ 
    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }

    /* Go ahead to read the VL string attribute. */
    astr_val_exp = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val_exp == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for expected buffer failed.\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "attr-%u", which);

    /* Construct the expected VL string value,depending if
     * it is the modified value or the original value. */ 
    if(vrfy_mod == true)  
        HDsprintf(astr_val_exp,"%u%c",which,'A');
    else 
        HDsprintf(astr_val_exp,"%u",which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which,
        which);

    dbgf(1,"expected vl attr is= %s\n",astr_val_exp);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

    /* Create a VL string datatype  */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED(); AT();
        printf("Cannot create variable length datatype.\n");
        goto error;
    }

    if (H5Tset_size(atype, H5T_VARIABLE) < 0) {
        H5_FAILED(); AT();
        printf("Cannot set variable length datatype.\n");
        goto error;
    }

    if (H5Aread(aid, atype, &astr_val) < 0) {
        H5_FAILED(); AT();
        printf("Cannot read the attribute.\n");
        goto error;
    }

    dbgf(1,"read attr is= %s\n",astr_val);
    if (HDstrcmp(astr_val, astr_val_exp) != 0) {
        H5_FAILED(); AT();
        dbgf(2, "reader: the vl add_attribute verfication failed,expected value is  %s\n", astr_val_exp);
        dbgf(2, "reader: the vl add_attribute verfication failed, the value is %s\n", astr_val);
        printf("The vl add_attribute verification failed\n");
        goto error;
    }

    if(H5Tclose(atype) <0) {
        H5_FAILED(); AT();
        printf("H5Tclose failed.\n");
        goto error;
    }
 
    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose failed.\n");
        goto error;
    }

    H5free_memory(astr_val);
    HDfree(astr_val_exp);

    /* Reader sends an OK message back to the reader */
    if(s->use_named_pipes && s->attr_test == true) {
        if(np_rd_send(s)==false) 
            goto error2;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(atype);
    } H5E_END_TRY;
    if(astr_val) 
        H5free_memory(astr_val);
    if(astr_val_exp)
        HDfree(astr_val_exp);

    /* The reader sends an error message to the writer to stop the test.*/
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);

error2:

    return false;

}

/*-------------------------------------------------------------------------
 * Function:    verify_del_one_attr
 *
 * Purpose:     verify if an attribute is successfully deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              const char* aname
 *              The name of the attribute to be deleted.
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is an internal function used by "remove-vlstr",
 *              "compact-del","dense-del",dense-del-to-compact"  tests.
 *-------------------------------------------------------------------------
*/ 


static bool
verify_del_one_attr(state_t *s,hid_t g, const char *aname) {

    htri_t attr_exists = FALSE;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message 
     * is an error message.
     */ 
    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }

    /* Check if the deleted attribute still exists. */
    attr_exists = H5Aexists_by_name(g,".",aname,H5P_DEFAULT);
    if(attr_exists == FALSE) { 
        dbgf(1,"verify_del_attrs_compact() test: \n");
        dbgf(1,"  attribute %s is successfully deleted. \n",aname);
    }
    else if(attr_exists == TRUE) {
        dbgf(1,"verify_del_attrs_compact() test failed \n");
        goto error;
    }
    else{
        dbgf(1,"H5Aexists_by_name failed \n");
        goto error;
    }

    /* Reader sends an OK message back to the reader */
    if(s->use_named_pipes && s->attr_test == true) {
        if(np_rd_send(s)==false) 
            goto error;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;
error: 
    /* The reader sends an error message to the writer to stop the test.*/
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,false);

error2:
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_remove_vlstr_attr
 *
 * Purpose:     Verify if an variable length string attribute is   
 *              successfully deleted by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              the attribute name.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is for the "remove-vstr" test.
 *              Also note this function first verifies if
 *              a variable length attribute is added then
 *              it verifies if it is deleted successfully.
 *-------------------------------------------------------------------------
*/ 

static bool
verify_remove_vlstr_attr(state_t* s,hid_t g, unsigned int which)
{
    bool ret = false;
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format = "attr-%u";

    ret = verify_group_vlstr_attr(s,g,which,false);
    if(ret == true) {
        HDsprintf(attrname,aname_format,which);
        ret = verify_del_one_attr(s,g,attrname);
    }
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_modify_vlstr_attr
 *
 * Purpose:     Verify if an variable length string attribute is   
 *              successfully modified by the writer.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              the attribute name.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is for the "modify-vstr" test.
 *              Also note this function first verifies if
 *              a variable length attribute is added then
 *              it verifies if it is modified successfully.
 *-------------------------------------------------------------------------
*/ 

static bool 
verify_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which){

    bool ret = false;

    ret = verify_group_vlstr_attr(s,g,which,false);
    if(ret == true) 
        ret = verify_group_vlstr_attr(s,g,which,true);
    return ret;

}

/*-------------------------------------------------------------------------
 * Function:    verify_attrs_compact
 *
 * Purpose:     verify if attributes are successfully added for the compact
 *              storage.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigend max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct the 
 *              attribute names. 
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact" test.
 *-------------------------------------------------------------------------
*/ 

static bool 
verify_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    unsigned u;
    bool ret = true;
    const char* aname_format = "attr-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];

    /* Need to verify the added attribute one by one. */
    for (u = 0; u < max_c; u++) {

        HDsprintf(attrname, aname_format, which,u);
        if(false == vrfy_attr(s,g,u+which,attrname,which)) {
            ret = false;
            break;
        }

    }
    return ret;

}

/*-------------------------------------------------------------------------
 * Function:    verify_attrs_compact_dense
 *
 * Purpose:     verify if attributes are successfully added first in the 
 *              compact storage then in the dense storage.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigend max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact-dense" test.
 *-------------------------------------------------------------------------
*/ 


static bool 
verify_attrs_compact_dense(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    const char* aname_format = "attr-d-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];

    bool ret = verify_attrs_compact(s,g,max_c,which);

    if(ret == true) { 

        /* Now the storage is in dense. Verify if the
         * retrieved value is correct. */
        HDsprintf(attrname, aname_format, max_c+which,0);
        ret = vrfy_attr(s,g,which+max_c,attrname,which);
        if(ret == false) 
            dbgf(1,"verify_attrs_compact_dense failed \n");

    }
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact
 *
 * Purpose:     verify if an attribute in compact storage is successfully 
 *              deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigend max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "compact-del" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              it verifies if one added attribute is deleted  successfully.
 *-------------------------------------------------------------------------
*/ 

static bool 
verify_del_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    const char* aname_format = "attr-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];

    bool ret = verify_attrs_compact(s,g,max_c,which);

    if(ret == true) { 
        /* The writer only deletes the attribute attr-which-0 */
        HDsprintf(attrname,aname_format,which,0);
        ret = verify_del_one_attr(s,g,attrname);
    }

    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact_dense
 *
 * Purpose:     verify if an attribute in dense storage is successfully 
 *              deleted.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigend max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "dense-del" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              in dense storage. Afterwards, 
 *              it verifies if one added attribute is deleted successfully.
 *-------------------------------------------------------------------------
*/ 


static bool 
verify_del_attrs_compact_dense(state_t *s, 
                               hid_t g, 
                               unsigned max_c, 
                               unsigned int which) {

    const char* aname_format = "attr-d-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];

    bool ret = verify_attrs_compact_dense(s,g,max_c,which);

    if(ret == true) { 
        /* The writer only deletes the attribute attr-d-which-0 */
        HDsprintf(attrname,aname_format,max_c+which,0);
        ret = verify_del_one_attr(s,g,attrname);
    }

    return ret;

}
 
/*-------------------------------------------------------------------------
 * Function:    verify_del_attrs_compact_dense_compact
 *
 * Purpose:     verify that the attributes are deleted successfully
 *              even the attribute storage changes from dense to
 *              compact.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters 
 *
 *              hid_t g
 *              HDF5 object ID (in this file: means group ID)
 *
 *              unsigend max_c
 *              The maximal number of attributes the compact storage
 *              can hold
 *
 *              unsigend min_d
 *              The minimal number of attributes to be stored in
 *              dense storage
 *
 *              unsigned int which
 *              Use to derieve the expected attribute value added 
 *              by the writer. It is also used to construct
 *              attribute names.
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This function is used by the "dense-del-to-compact" test.
 *              Also note this function first verifies if
 *              attributes are successfully added in compact storage then
 *              in dense storage. Afterwards, 
 *              it verifies if some added attributes are deleted successfully
 *              until the storage changes from dense to compact.
 *-------------------------------------------------------------------------
*/ 


static bool 
verify_del_attrs_compact_dense_compact(state_t *s, 
                                       hid_t g, 
                                       unsigned max_c, 
                                       unsigned min_d, 
                                       unsigned int which) {

    unsigned u;
    const char* aname_format = "attr-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];

    /* Verify the attributes are added correctly from
     * compact to dense storage*/
    bool ret = verify_attrs_compact_dense(s,g,max_c,which);

    if(ret == true) { 

        /* Then verify the deletion of attributes 
         * from dense to compact.
         */
        u = max_c + 1;
        for(u--;u>=(min_d-1);u--) {
            HDsprintf(attrname, aname_format, which,max_c-u);
            ret = verify_del_one_attr(s,g,attrname);
        }

        /* Just verify one more deleted attribute by the writer.
           The storage is still compact. */
        HDsprintf(attrname,aname_format,max_c+which,0);
        ret = verify_del_one_attr(s,g,attrname);
    }

    return ret;

}

/*-------------------------------------------------------------------------
 * Function:    verify_group_attribute
 *
 * Purpose:     Check the attribute test pattern and then call the
 *              correponding verification function.
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
 *              group and attribute names. 
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the verify_group() function.
 *-------------------------------------------------------------------------
*/ 


static bool
verify_group_attribute(state_t *s, hid_t g, unsigned int which)
{
    char test_pattern = s->at_pattern;
    bool ret = false;
    unsigned max_compact = 0;
    unsigned min_dense = 0;
    hid_t gcpl = H5I_INVALID_HID;

    /* For tests "compact","compact-to-dense","compact-del",
     *           "dense-del", "dense-del-to-compact",
     *     the maximal number of attributes for the compact storage
     *     and the minimal number of attributes for the dense storage
     *     are needed. So obtain them here */
    switch (test_pattern) {
        case 'c':
        case 't':
        case 'C':
        case 'D':
        case 'T':
            if((gcpl = H5Gget_create_plist(g)) < 0) {
                H5_FAILED(); AT();
                printf("H5Gget_create_plist failed\n");
                goto error;
            }
            if (H5Pget_attr_phase_change(gcpl,&max_compact,&min_dense) < 0) {
                H5_FAILED(); AT();
                printf("H5Pget_attr_phase_change failed\n");
                goto error;
            }
            if(H5Pclose(gcpl) < 0) {
                H5_FAILED(); AT();
                printf("H5Pclose failed\n");
                goto error;
            }
            break;
        case 'v':
        case 'd':
        case 'M':
        case 'm':
        case 'r':
        case ' ':
        default:
           break;
    }

    /* Distribute the verification test. */
    switch (test_pattern) {
        case 'c':
            ret = verify_attrs_compact(s, g, max_compact, which);
            break;
        case 't':
            ret = verify_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'C':
            ret = verify_del_attrs_compact(s, g, max_compact, which);
            break;
        case 'D':
            ret = verify_del_attrs_compact_dense(s, g, max_compact, which);
            break;
        case 'T':
            ret = verify_del_attrs_compact_dense_compact(s, g, max_compact, min_dense, which);
            break;
        case 'M':
            ret = verify_modify_attr(s, g,  which);
            break;
        case 'v':
            ret = verify_group_vlstr_attr(s,g, which,false);
            break;
        case 'r':
            ret = verify_remove_vlstr_attr(s,g,  which);
            break;
        case 'm':
            ret = verify_modify_vlstr_attr(s,g,  which);
            break;
        case 'd':
        case ' ':
        default:
            ret = verify_default_group_attr(s, g, which);
            break;
    }

    return ret;

error:
    /* Still to finish the handshaking */
    if(s->use_named_pipes && s->attr_test == true) { 
        np_rd_receive(s);
        np_send_error(s,false);
    }
    return false;
}

/*-------------------------------------------------------------------------
 * Function:    verify_group
 *
 * Purpose:     verify the success of group creation and 
 *              carry out the test for attribute operations(add,delete etc.)
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
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    /* The reader receives a message from the writer.Then sleep
     * for a few ticks or stop the test if the received message 
     * is an error message.
     */ 
    if(s->use_named_pipes && true == s->attr_test) {

        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n",s->np_notify);

    }

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("Number of created groups is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

 
    if((g = H5Gopen(s->file, name, H5P_DEFAULT)) <0) {
        H5_FAILED(); AT();
        printf("H5Gopen failed\n");
        goto error;
    }

    /* Reader sends an OK message back to the reader */
    if(s->use_named_pipes && s->attr_test == true) {

        if(np_rd_send(s)==false) 
            goto error;
        dbgf(1, "Reader: finish sending back the message: %d\n",s->np_notify);
        
    }

    /* Check if we need to skip the attribute test for this group. */
    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(s, g, which);
    else
        result = true;

    if (H5Gclose(g) < 0) {
        H5_FAILED(); AT();
        printf("H5Gclose failed\n");
        goto error;
    }

    return result;

error:

    H5E_BEGIN_TRY {
        H5Gclose(g);
    } H5E_END_TRY;

    /* The reader sends an error message to the writer to stop the test.*/
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,false);

error2:

    return false;

}

int
main(int argc, char **argv)
{
    hid_t fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned step;
    bool writer = false;
    state_t s;
    const char *personality;
    H5F_vfd_swmr_config_t config;
    const char *fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char *fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int fd_writer_to_reader = -1, fd_reader_to_writer = -1;
    int notify = 0, verify = 0;
    bool wg_ret = false;
    bool vg_ret = false;

    if (!state_init(&s, argc, argv)) {
        H5_FAILED(); AT();
        printf("state_init failed\n");
        goto error;
    }

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL &&
        strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL &&
             strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        H5_FAILED(); AT();
        printf("unknown personality, expected vfd_swmr_group_{reader,writer}\n");
        goto error;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, FALSE, 128, "./group-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config)) < 0) {
        H5_FAILED(); AT();
        printf("vfd_swmr_create_fapl failed\n");
        goto error;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
        H5_FAILED(); AT();
        printf("H5Pcreate failed\n");
        goto error;
    }

    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1) < 0) {
        H5_FAILED(); AT();
        printf("H5Pset_file_space_strategy failed\n");
        goto error;
    }

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file < 0) {
        H5_FAILED(); AT();
        printf("H5Fcreate/open failed\n");
        goto error;
    }

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (s.use_named_pipes && writer) {
        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed\n");
            goto error;
        }

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed\n");
            goto error;
        }

    }

    /* Both the writer and reader open the pipes */
    if (s.use_named_pipes && (fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("HDopen failed\n");
        goto error;
    }

    if (s.use_named_pipes && (fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("HDopen failed\n");
        goto error;
    }

    /* Pass the named pipe information to the struct of state_t s, for attribute tests.*/
    if(s.use_named_pipes) {
        s.np_fd_w_to_r = fd_writer_to_reader;
        s.np_fd_r_to_w = fd_reader_to_writer;
        s.np_notify = notify;
        s.np_verify = verify;
        s.tick_len  = config.tick_len;
        s.max_lag   = config.max_lag;
    }

    /* For attribute test, force the named pipe to communicate in every step. */
    if (s.at_pattern != ' ') {
       s.attr_test = true;
       if(s.use_named_pipes)
            s.csteps = 1;
    }

    if (writer) {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            wg_ret = write_group(&s, step);

            if(wg_ret == false)  {
                H5_FAILED(); AT();
                printf("write_group failed at step %d\n",step);

                /* At communication interval, notifies the reader about the failture and quit */
                if (s.use_named_pipes && s.attr_test !=true && step % s.csteps == 0) 
                    np_send_error(&s,true);
                goto error;
            }
            else {

                /* At communication interval, notifies the reader and waits for its response */
                if (s.use_named_pipes && s.attr_test != true  && step % s.csteps == 0) {

                    if(np_wr_send_receive(&s) == false) {
                        H5_FAILED(); AT();
                        dbgf(2, "writer: write group - verification failed.\n");
                        goto error;
                    }
                }
            }
        }
    } else {
        for (step = 0; step < s.nsteps;step++) {
            dbgf(1, "reader: step %d\n", step);

            /* At communication interval, waits for the writer to finish creation before starting verification */
            if (s.use_named_pipes && s.attr_test != true && step % s.csteps == 0) {
                if(false == np_rd_receive(&s)) {
                    H5_FAILED(); AT();
                    goto error;
                }
            }

            /* For the default test, wait for a few ticks for the update to happen */
            if(s.use_named_pipes && s.attr_test== false) 
                decisleep(config.tick_len * s.update_interval);

            vg_ret = verify_group(&s, step);

            if (vg_ret == false) {

                printf("verify_group failed\n");
                H5_FAILED(); AT();

                /* At communication interval, tell the writer about the failure and exit */
                if (s.use_named_pipes && s.attr_test != true && step % s.csteps == 0) 
                    np_send_error(&s,false);
                goto error;

            }
            else {

                /* Send back the same nofity value for acknowledgement to tell the writer
                 * move to the next step. */
                if (s.use_named_pipes && s.attr_test!=true && step % s.csteps == 0) {
                    if(np_rd_send(&s)==false) 
                        goto error;
                }
            }
           
        }
    }

    if (H5Pclose(fapl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed\n");
        goto error;
    }

    if (H5Pclose(fcpl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed\n");
        goto error;
    }

    if (H5Fclose(s.file) < 0) {
        H5_FAILED(); AT();
        printf("H5Fclose failed\n");
        goto error;
    }

    /* Both the writer and reader close the named pipes */
    if (s.use_named_pipes && HDclose(fd_writer_to_reader) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    if (s.use_named_pipes && HDclose(fd_reader_to_writer) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    /* Reader finishes last and deletes the named pipes */
    if(s.use_named_pipes && !writer) {
        if(HDremove(fifo_writer_to_reader) != 0) {
            H5_FAILED(); AT();
            printf("HDremove failed\n");
            goto error;
        }

        if(HDremove(fifo_reader_to_writer) != 0) {
            H5_FAILED(); AT();
            printf("HDremove failed\n");
            goto error;
        }
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(s.file);
    } H5E_END_TRY;

    if (s.use_named_pipes && fd_writer_to_reader >= 0)
        HDclose(fd_writer_to_reader);

    if (s.use_named_pipes && fd_reader_to_writer >= 0)
        HDclose(fd_reader_to_writer);

    if(s.use_named_pipes && !writer) {
        HDremove(fifo_writer_to_reader);
        HDremove(fifo_reader_to_writer);
    }

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
