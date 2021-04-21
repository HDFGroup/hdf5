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
                "    [-n iterations] [-N] [-q] [-u numb_ticks]\n"
        "\n"
        "-S:             do not use VFD SWMR\n"
        "-a steps:       `steps` between adding attributes\n"
        "-b:             write data in big-endian byte order\n"
        "-c steps:       `steps` between communication between the writer and reader\n"
        "-n ngroups:     the number of groups\n"
        "-N:             do not use named pipes, mainly for running the writer and reader seperately\n"
        "-u numb_tcks:   `numb_ticks` for the reader to wait before verification\n"
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

#if 0
    char tfile[PATH_MAX];
    char *end;

    *s = ALL_HID_INITIALIZER;
    esnprintf(tfile, sizeof(tfile), "%s", argv[0]);
    esnprintf(s->progname, sizeof(s->progname), "%s", HDbasename(tfile));
#endif

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

static bool np_rd_send(state_t *s) {

    if (HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED(); AT();
        printf("HDwrite failed\n");
        return false;
    }
    else
        return true;
}

static void np_send_error(state_t *s,bool writer) {
    s->np_notify = -1;
    if(writer) 
        HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int));
    else 
        HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int));
}

static bool 
add_attr(state_t *s, hid_t oid,unsigned int which,unsigned num_attrs,const char*aname_fmt) {

    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    //char* attrname_base= "attr-%u-%u";
    unsigned u;
    //int i;
    unsigned attr_value;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;
    hid_t atype = s->filetype;
    hid_t sid = s->one_by_one_sid;

// Just for debugging
#if 0
if(which == 1)
    goto error;
#endif

    if((amtype = H5Tget_native_type(atype,H5T_DIR_ASCEND)) <0) {
        H5_FAILED(); AT();
        printf("H5Tget_native_type failed\n");
        goto error;
    }

    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        //HDsprintf(attrname, "attr-%u-%u", which,u);
        //HDsprintf(attrname, attrname_base, which,u);
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

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, which, u+which);
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

        if (s->use_named_pipes && s->attr_test == true) {
            dbgf(2, "writer: write attr - ready to send the message: %d\n", s->np_notify+1);
            if(np_wr_send_receive(s) == false) {
                H5_FAILED(); AT();
                dbgf(2, "writer: write attr - verification failed.\n");
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
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,true);

error2: 
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(amtype);
    } H5E_END_TRY;

///dbgf(2, "writer: LEAVE FUNC: write attr - verification failed.\n");
    return false;

}

// Temp named pipe works.
#if 0
static bool 
add_attr(state_t *s, hid_t oid,unsigned int which,unsigned num_attrs,const char*aname_fmt,bool sl) {

    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    //char* attrname_base= "attr-%u-%u";
    unsigned u;
    int i;
    unsigned attr_value;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;
    hid_t atype = s->filetype;
    hid_t sid = s->one_by_one_sid;

    if((amtype = H5Tget_native_type(atype,H5T_DIR_ASCEND)) <0) {
        H5_FAILED(); AT();
        printf("H5Tget_native_type failed\n");
        goto error;
    }

    /* Add attributes, until just before converting to dense storage */
    for (u = 0; u < num_attrs; u++) {

        /* Create attribute */
        //HDsprintf(attrname, "attr-%u-%u", which,u);
        //HDsprintf(attrname, attrname_base, which,u);
        HDsprintf(attrname, aname_fmt, which,u);
        if((aid = H5Acreate2(oid, attrname, atype, sid, H5P_DEFAULT, 
            H5P_DEFAULT)) < 0) { 
            H5_FAILED(); AT();
            printf("H5Acreate2 failed\n");
            goto error;
        }

        attr_value = u+which;

        dbgf(1, "setting attribute %s on group %u to %u\n", attrname, which, u+which);
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

             if (s->attr_test == true) {
                /* Bump up the value of notify to notice the reader to start to read */
                s->np_notify++;
                
                
                dbgf(2, "writer: ready to send the message: %d.\n", s->np_notify);

                if (HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");

                /* During the wait, writer makes repeated HDF5 API calls
                 * to trigger EOT at approximately the correct time */
                for(i = 0; i < s->max_lag + 1; i++) {
                    decisleep(s->tick_len);
                    H5Aexists(s->file, "nonexistent");
                }

                /* Receive the same value from the reader and verify it before
                 * going to the next step */
                (s->np_verify)++;
                dbgf(2, "writer: ready to receive the message: %d.\n", s->np_verify);
                if (HDread(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "read failed");

                dbgf(2, "writer: finish receiving the message: %d.\n", s->np_notify);
                if (s->np_notify != s->np_verify)
                    errx(EXIT_FAILURE, "received message %d, expecting %d", s->np_notify, s->np_verify);
            }
       
    } /* end for */

    H5Tclose(amtype);
}
#endif

static bool 
modify_attr(state_t *s, hid_t g, const char* aname_fmt,unsigned int which) {

    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    hid_t aid = H5I_INVALID_HID;   
    hid_t amtype = H5I_INVALID_HID;
    int modify_value;

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
 
#if 0
    // Add later.
    //H5T_sign_t h5t_sign = H5Tget_sign(amtype);
    /* Unlikely, still make sure -no overflow. */
    if((unsigned int)((int)which)!=which) {
        printf("the number of %u causes of overflow when casted to an integer\n",which);  
        printf("number of iteration is too big, it causes overflow\n");
        goto error;
    }
#endif
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

    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: modify attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
                H5_FAILED(); AT();
                dbgf(2, "writer: write attr - verification failed.\n");
                goto error2;
        }
    }

    return true;
error:
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Tclose(aid);
    } H5E_END_TRY;

error2:
    return false;

}


#if 0
static bool 
temp_add_default_group_attr(state_t *s, hid_t g, unsigned int which) {

    const char* aname_format ="attr-%u";

    add_attr(s,g,which,1,aname_format,false);

}
#endif


static bool 
add_default_group_attr(state_t *s, hid_t g, unsigned int which) {

    const char* aname_format ="attr-%u";

    return add_attr(s,g,which,1,aname_format);

}


static bool 
add_vlstr_attr(state_t*s, hid_t g, unsigned int which) {

    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    char name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;
    hid_t sid = s->one_by_one_sid;

    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for buffer failed.\n");
        goto error;
    }

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

    if ((aid = H5Acreate2(g, name, atype, sid, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Acreate2 failed.\n");
        goto error;
    }

    dbgf(1, "astr_val is  %s \n", astr_val);
    //if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
    //if (H5Awrite(aid, H5T_NATIVE_UINT, astr_val) < 0)
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

static bool 
del_one_attr(state_t *s, hid_t obj_id,bool is_dense,bool is_vl,unsigned int which) {

    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format_d = "attr-d-%u-%u";
    const char* aname_format = "attr-%u-%u";
    const char* aname_format_vl="attr-%u";

    
    dbgf(2, "writer: coming to delete the attribute.\n");
    if(is_dense == true)  
       HDsprintf(attrname, aname_format_d, which,0);
    else if(is_vl == true)
       HDsprintf(attrname, aname_format_vl, which,0);
    else
       HDsprintf(attrname, aname_format, which,0);
    
    /* Delete attribute */
    if(H5Adelete(obj_id, attrname) <0) {
        H5_FAILED(); AT();
        printf("H5Adelete() failed\n");
        goto error;
    }

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


static bool
add_del_vlstr_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;

    ret_value = add_vlstr_attr(s,g,which);
    if(ret_value == true) 
        ret_value = del_one_attr(s,g,false,true,which);

    return ret_value;

}

static bool 
modify_vlstr_attr(state_t*s,hid_t g, unsigned int which) {

    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    //char name[sizeof("attr-9999999999")];
    char name[VS_ATTR_NAME_LEN];
    char *astr_val = NULL;

    //astr_val = malloc(sizeof("9999999999!"));
    astr_val = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for buffer failed.\n");
        goto error;
    }

    HDsprintf(astr_val,"%u%c",which,'A');
    //const char *astr_val="test";
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

    //if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
    if ((aid = H5Aopen(g, name, H5P_DEFAULT))<0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed.\n");
        goto error;
    }

    dbgf(1, "astr_val is  %s \n", astr_val);

    //if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
    //if (H5Awrite(aid, H5T_NATIVE_UINT, astr_val) < 0)
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

static bool
add_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;
    //const char* aname_format ="attr-%u";
    ret_value = add_vlstr_attr(s,g,which);
    if (true == ret_value) 
        ret_value = modify_vlstr_attr(s,g,which);

    return ret_value;
}

static bool 
add_attrs_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    unsigned max_compact = 0;
    unsigned min_dense = 0;
    const char* aname_format="attr-%u-%u";
    
    if(H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense)<0) {
        H5_FAILED(); AT();
        printf("H5Pget_attr_phase_change() failed\n");
        goto error;
    }
    
    /* Add attributes, until just before converting to dense storage */
    return add_attr(s,g,which,max_compact,aname_format);


error:
    return false;
}

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
        ret_value = add_attr(s,g,which+max_compact,1,aname_format);

    return ret_value;

error:
    return false;
}

#if 0
static bool 
del_one_attr(state_t *s, hid_t obj_id,bool is_dense,unsigned int which) {

    char attrname[sizeof("attr-d-9999999999-999")];
    const char* aname_format_d = "attr-d-%u-%u";
    const char* aname_format = "attr-%u-%u";

    if(is_dense == true)  
       HDsprintf(attrname, aname_format_d, which,0);
//printf("attrname is %s\n",attrname);
    else 
       HDsprintf(attrname, aname_format, which,0);
    
    /* Delete attribute */
    H5Adelete(obj_id, attrname);
    nanosleep(&(s->update_interval), NULL); 


}
#endif

static bool
del_attrs_compact_dense_compact(state_t *s, hid_t obj_id,hid_t gcpl,unsigned int which) {
    
    unsigned max_compact = 0;
    unsigned min_dense = 0;
    unsigned u = 0;

    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format="attr-%u-%u";
    const char* adname_format="attr-d-%u-%u";

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
    for(u--;u>=(min_dense-1);u--) {
        HDsprintf(attrname, aname_format, which,max_compact-u);
        if (H5Adelete(obj_id,attrname) < 0) {
            H5_FAILED(); AT();
            printf("H5Adelete failed\n");
            goto error;
        }
        if(s->use_named_pipes && s->attr_test == true) {
            dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify+1);
            if(np_wr_send_receive(s) == false) {
                H5_FAILED(); AT();
                dbgf(2, "writer: delete attr - verification failed.\n");
                goto error2;
            }
        }
    }

    // The writer only deletes the attribute attr-which-0
    HDsprintf(attrname,adname_format,max_compact+which,0);
    /// CHECK HERE, add H5Adelete()
    if (H5Adelete(obj_id,attrname) < 0) {
            H5_FAILED(); AT();
            printf("H5Adelete failed\n");
            goto error;
    }
    if(s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: delete attr - ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            dbgf(2, "writer: delete attr - verification failed.\n");
            goto error2;
        }
    }

    
   
// May H5Oget_info3 -- obtain the number of attributes. 
//Check the number of attributes >=min_dense. 
//We may use the internal function  
//is_dense = H5O__is_attr_dense_test(dataset) to check if it is dense in the future. 
//
    return true;

error: 
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);

error2:
    return false;
}


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
    return false;

}

static bool 
add_del_attrs_compact_dense_compact(state_t *s, hid_t g, hid_t gcpl, unsigned int which) {

    bool ret_value = false;
    ret_value = add_attrs_compact_dense(s,g,gcpl,which);
    if(ret_value == true) 
        ret_value = del_attrs_compact_dense_compact(s,g,gcpl,which);
    

    return ret_value;
}

static bool
add_modify_default_group_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret_value = false;
    const char* aname_format ="attr-%u";
    ret_value = add_default_group_attr(s,g,which);
    if(ret_value == true)
        ret_value = modify_attr(s,g,aname_format,which);
    return ret_value;

} 

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
//printf("add_group_attribute return value %d\n",(int)ret_value);
    return ret_value;

}

#if 0
{
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
            H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
        errx(EXIT_FAILURE, "H5Awrite failed");
    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");
}
#endif

static bool
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    hid_t gcpl = H5I_INVALID_HID;
    bool result = true;

    //assert(which < s->nsteps);
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

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, 
                        H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gcreate2 failed\n");
        //np_send_error(s,true);
        goto error;
    }

    /* If an attribute test is turned on, the NP writer sends a message */
    if (s->use_named_pipes && s->attr_test == true) {
        dbgf(2, "writer: ready to send the message: %d\n", s->np_notify+1);
        if(np_wr_send_receive(s) == false) {
            H5_FAILED(); AT();
            goto error2;
        }
    }
#if 0
dbgf(1,"Writer: pass group creation\n");        
#endif
    if (s->asteps != 0 && which % s->asteps == 0) 
        result = add_group_attribute(s, g, gcpl,which);
#if 0
if(result == true) 
printf("Group: successfully receiving the verification from the reader.\n");    
else
printf("Group: Fail to receive the verficiation from the reader.\n");
#endif
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
    // Consistent
    // But if receiving an error message,no need to send the error again.
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,true);

error2: 

    H5E_BEGIN_TRY {
        H5Gclose(g);
        H5Pclose(gcpl);
    } H5E_END_TRY;

    return false;

}

// Temp name pipe works.
#if 0
static bool
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g;
    hid_t gcpl;
    int i;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    if(gcpl <0) 
        errx(EXIT_FAILURE, "H5Pcreate failed");
    if(s->at_pattern =='d') 
        H5Pset_attr_phase_change(gcpl, 0, 0);

    g = H5Gcreate2(s->file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT);
    // TODO: before the failure, needs to check if the reader is waiting for the pipe.
    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate(, \"%s\", ) failed", name);
             if (s->attr_test == true) {
                /* Bump up the value of notify to notice the reader to start to read */
                s->np_notify++;
                
                
                dbgf(2, "writer: ready to send the message: %d.\n", s->np_notify);

                if (HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");

                /* During the wait, writer makes repeated HDF5 API calls
                 * to trigger EOT at approximately the correct time */
                for(i = 0; i < s->max_lag + 1; i++) {
                    decisleep(s->tick_len);
                    H5Aexists(s->file, "nonexistent");
                }

                /* Receive the same value from the reader and verify it before
                 * going to the next step */
                (s->np_verify)++;
                dbgf(2, "writer: ready to receive the message: %d.\n", s->np_verify);
                if (HDread(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "read failed");

                dbgf(2, "writer: finish receiving the message: %d.\n", s->np_notify);
                if (s->np_notify != s->np_verify)
                    errx(EXIT_FAILURE, "received message %d, expecting %d", s->np_notify, s->np_verify);
            }

    if (s->asteps != 0 && which % s->asteps == 0) {
        add_group_attribute(s, g, gcpl,which);
    }
    

    if(H5Pclose(gcpl) <0) 
        errx(EXIT_FAILURE, "H5Pcreate failed");

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");

}

#endif


static bool
vrfy_attr(state_t *s, hid_t g, unsigned int which,  char* aname) {

    unsigned int read_which;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;

    //char name[sizeof("attr-d-9999999999-999")];

    //esnprintf(name, sizeof(name), "attr-%u", which);
    //esnprintf(name, sizeof(name), aname_fmt, which);

    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }

    dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, which,
        which);


#if 0
    if (H5Sget_simple_extent_npoints(s->one_by_one_sid)!=1) {
        dbgf(1, "The number of elements of %s on group %u should be 1, exit.\n"
             name,which);
        restore_estack(es);
        return false;
    }
#endif

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
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);
error2:
    return false;

}

// Temp name pipe works
#if 0
static bool
vrfy_attr(state_t *s, hid_t g, unsigned int which,  char* aname, bool sl) {

    estack_state_t es;
    unsigned int read_which;
    hid_t aid;
    hid_t amtype;
    bool ret_value = true;
    //char name[sizeof("attr-d-9999999999-999")];

    //esnprintf(name, sizeof(name), "attr-%u", which);
    //esnprintf(name, sizeof(name), aname_fmt, which);
    dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, which,
        which);

    if(true == s->attr_test) {
        s->np_verify++;
        /* Receive the notify that the writer bumped up the value */
        if (HDread(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0)
            err(EXIT_FAILURE, "read failed");
        if (s->np_notify != s->np_verify)
            errx(EXIT_FAILURE, "received message %d, expecting %d", s->np_notify, s->np_verify);
        decisleep(3*(s->tick_len));
     dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);

   }
    
    es = disable_estack();

#if 0
    if (H5Sget_simple_extent_npoints(s->one_by_one_sid)!=1) {
        dbgf(1, "The number of elements of %s on group %u should be 1, exit.\n"
             name,which);
        restore_estack(es);
        return false;
    }
#endif

    amtype = H5Tget_native_type(s->filetype,H5T_DIR_ASCEND);
    if ((aid = H5Aopen(g, aname, H5P_DEFAULT)) < 0) {
        restore_estack(es);
        ret_value = false;
    }

    if(ret_value == true) {
    if (H5Aread(aid, amtype, &read_which) < 0) {
        restore_estack(es);
        if (H5Aclose(aid) < 0)
            errx(EXIT_FAILURE, "H5Aclose failed");
        ret_value =  false;
    }
    }

    restore_estack(es);

    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");

    if(ret_value == true) 
        ret_value = (read_which == which);

    if(s->attr_test == true) {
    if(ret_value == false) {
                    dbgf(2, "reader: the add_attribute verfication failed %d\n", which);
                    dbgf(2, "reader: the add_attribute verfication failed, the value is %d\n", read_which);
                    
                    s->np_notify = 0;
    }
                if (HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");

                    dbgf(2, "reader: finish sending back the message: %d\n.", s->np_notify);
    }
 
    return ret_value;
     
}
#endif

static bool
verify_default_group_attr(state_t*s,hid_t g, unsigned int which)
{
    //char attrname[sizeof("attr-9999999999")];
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format = "attr-%u";
    //bool ret_value = false;
    HDsprintf(attrname, aname_format, which);
    return vrfy_attr(s,g,which,attrname);

}


static bool
verify_modify_attr(state_t *s, hid_t g, unsigned int which) {

    bool ret = false;
    const char* aname_fmt ="attr-%u";
    estack_state_t es;
    int read_which;
    hid_t aid = H5I_INVALID_HID;
    hid_t amtype = H5I_INVALID_HID;
    //char aname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];

    ret = verify_default_group_attr(s,g,which);
 
    if(ret == true) {
        if(s->use_named_pipes && true == s->attr_test) {
            if(false == np_rd_receive(s)) {
                H5_FAILED(); AT();
                goto error2;
            }
            decisleep(s->tick_len * s->update_interval);
            dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
        }

        esnprintf(attrname, sizeof(attrname), aname_fmt, which);
    //dbgf(1, "verifying attribute %s on group %u equals %u\n", aname, which,
    //    which);

#if 0
    if (H5Sget_simple_extent_npoints(s->one_by_one_sid)!=1) {
        dbgf(1, "The number of elements of %s on group %u should be 1, exit.\n"
             name,which);
        restore_estack(es);
        return false;
    }
#endif

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

#if 0
        if((unsigned int)((int)which)!=which) {
            H5_FAILED(); AT();
            printf("the unsigned %u causes overflow when casted to signed.\n",which);  
            printf("number of iteration is too big, it causes overflow.\n");
            goto error;
        }
#endif

        if(read_which != (which+10000)) {
            H5_FAILED(); AT();
            dbgf(2, "reader: the modified_attr() expected value is  %d\n", (-1)*(int)which);
            dbgf(2, "reader: the modified_attr() actual value is %d\n", read_which);
            printf("The modify_attribute verification failed.\n");
            goto error;
        }

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

    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);

error2:

    return false;
}

#if 0
static bool
verify_default_group_attr(state_t*s,hid_t g, unsigned int which)
{
    char attrname[sizeof("attr-9999999999")];
    const char* aname_format = "attr-%u";
    HDsprintf(attrname, aname_format, which);
    return vrfy_attr(s,g,which,attrname,false);
}
#endif

static bool
verify_group_vlstr_attr(state_t*s, hid_t g, unsigned int which, bool vrfy_mod)
{
    estack_state_t es;
    //unsigned int read_which;
    bool ret = false;
    hid_t aid = H5I_INVALID_HID;
    hid_t atype = H5I_INVALID_HID;
    //char name[sizeof("attr-9999999999")];
    char name[VS_ATTR_NAME_LEN];

    char *astr_val_exp = NULL;
    char * astr_val = NULL;

    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }

#if 0
    astr_val = malloc(VS_ATTR_NAME_LEN);
    if (astr_val == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for buffer failed.\n");
        goto error;
    }
#endif

    astr_val_exp = HDmalloc(VS_ATTR_NAME_LEN);
    if (astr_val_exp == NULL) {
        H5_FAILED(); AT();
        printf("Allocate memory for expected buffer failed.\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "attr-%u", which);
    if(vrfy_mod == true)  
        HDsprintf(astr_val_exp,"%u%c",which,'A');
    else 
        HDsprintf(astr_val_exp,"%u",which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which,
        which);

    dbgf(1,"expected vl attr is= %s\n",astr_val_exp);

    es = disable_estack();
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

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

    //restore_estack(es);

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
    if(s->use_named_pipes && s->attr_test == true) 
        np_send_error(s,false);

error2:

    return false;

}

static bool
verify_del_one_attr(state_t *s,hid_t g, const char *aname) {

    //bool ret = false;
    htri_t attr_exists = FALSE;

    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error2;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);
    }


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

    if(s->use_named_pipes && s->attr_test == true) {
        if(np_rd_send(s)==false) 
            goto error;
        dbgf(2, "reader: finish sending back the message: %d\n", s->np_notify);
    }

    return true;
error: 
    if(s->use_named_pipes && s->attr_test == true)
        np_send_error(s,false);

error2:
    return false;
}

static bool
verify_remove_vlstr_attr(state_t* s,hid_t g, unsigned int which)
{
    estack_state_t es;
    bool ret = false;
    htri_t attr_exists = FALSE;
    //char attrname[sizeof("attr-9999999999")];
    char attrname[VS_ATTR_NAME_LEN];
    const char* aname_format = "attr-%u";

    ret = verify_group_vlstr_attr(s,g,which,false);
    if(ret == true) {
        HDsprintf(attrname,aname_format,which);
        // Add error handling later.
        ret = verify_del_one_attr(s,g,attrname);
#if 0
        es = disable_estack();
        attr_exists = H5Aexists_by_name(g,".",attrname,H5P_DEFAULT);
        restore_estack(es);

        if(attr_exists == FALSE) { 
            dbgf(1,"verify_remove_vlstr_attr test: \n");
            dbgf(1,"  attribute %s on group %u is successfully deleted. \n",attrname,which);
            ret = true;
        }
        else if(attr_exists == TRUE) {
            dbgf(1,"verify_remove_vlstr_attr test failed \n");
            ret = false;
        }
        else{
            dbgf(1,"H5Aexists_by_name failed \n");
            ret = false;
        }
 
#endif
    }
    return ret;
}

static bool 
verify_modify_vlstr_attr(state_t *s, hid_t g, unsigned int which){

    bool ret = false;

    // May change the sid with state_t s
    ret = verify_group_vlstr_attr(s,g,which,false);
    if(ret == true) 
        ret = verify_group_vlstr_attr(s,g,which,true);
    return ret;

}

static bool 
verify_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    unsigned u;
    bool ret = true;
    const char* aname_format = "attr-%u-%u";
    //char attrname[sizeof("attr-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    for (u = 0; u < max_c; u++) {

        HDsprintf(attrname, aname_format, which,u);
        if(false == vrfy_attr(s,g,u+which,attrname)) {
            ret = false;
            break;
        }

    }
    return ret;

}

static bool 
verify_attrs_compact_dense(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    const char* aname_format = "attr-d-%u-%u";
    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    bool ret = verify_attrs_compact(s,g,max_c,which);
    if(ret == true) { 
        //HDsprintf(attrname, aname_format, which,0);
        HDsprintf(attrname, aname_format, max_c+which,0);
        ret = vrfy_attr(s,g,which+max_c,attrname);
        if(ret == false) 
            dbgf(1,"verify_attrs_compact_dense failed \n");
    }
    return ret;
}

static bool 
verify_del_attrs_compact(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    estack_state_t es;
    htri_t attr_exists = FALSE;
    const char* aname_format = "attr-%u-%u";
    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    bool ret = verify_attrs_compact(s,g,max_c,which);
    if(ret == true) { 
        // The writer only deletes the attribute attr-which-0
        HDsprintf(attrname,aname_format,which,0);
        // Add error handling later.
        //es = disable_estack();
        ret = verify_del_one_attr(s,g,attrname);
    }
#if 0
        attr_exists = H5Aexists_by_name(g,".",attrname,H5P_DEFAULT);
        restore_estack(es);
        if(attr_exists == FALSE) { 
            dbgf(1,"verify_del_attrs_compact() test: \n");
            dbgf(1,"  attribute %s on group %u is successfully deleted. \n",attrname,which);
            ret = true;
        }
        else if(attr_exists == TRUE) {
            dbgf(1,"verify_del_attrs_compact() test failed \n");
            ret = false;
        }
        else{
            dbgf(1,"H5Aexists_by_name failed \n");
            ret = false;
        }
    }
#endif
    return ret;
}

static bool 
verify_del_attrs_compact_dense(state_t *s, hid_t g, unsigned max_c, unsigned int which) {

    estack_state_t es;
    htri_t attr_exists = FALSE;
    const char* aname_format = "attr-d-%u-%u";
    //char attrname[sizeof("attr-d-9999999999-999")];
    char attrname[VS_ATTR_NAME_LEN];
    bool ret = verify_attrs_compact_dense(s,g,max_c,which);
    if(ret == true) { 
        // The writer only deletes the attribute attr-which-0
        HDsprintf(attrname,aname_format,max_c+which,0);
        // Add error handling later.
        ret = verify_del_one_attr(s,g,attrname);
#if 0
        es = disable_estack();
        attr_exists = H5Aexists_by_name(g,".",attrname,H5P_DEFAULT);
        restore_estack(es);
        if(attr_exists == FALSE) { 
            dbgf(1,"verify_del_attrs_compact_dense() test: \n");
            dbgf(1,"  attribute %s on group %u is successfully deleted. \n",attrname,which);
            ret = true;
        }
        else if(attr_exists == TRUE) {
            dbgf(1,"verify_del_attrs_compact_dense() test failed \n");
            ret = false;
        }
        else{
            dbgf(1,"H5Aexists_by_name failed \n");
            ret = false;
        }
#endif
    }
    return ret;

}
static bool 
verify_del_attrs_compact_dense_compact(state_t *s, 
                                       hid_t g, 
                                       unsigned max_c, 
                                       unsigned min_d, 
                                       unsigned int which) {
    estack_state_t es;
    unsigned u;
    htri_t attr_exists = FALSE;
    const char* aname_format = "attr-%u-%u";
    char attrname[VS_ATTR_NAME_LEN];
    //char attrname[sizeof("attr-9999999999-999")];
    bool ret = verify_attrs_compact_dense(s,g,max_c,which);
    if(ret == true) { 
        u = max_c + 1;
        for(u--;u>=(min_d-1);u--) {
            HDsprintf(attrname, aname_format, which,max_c-u);
            ret = verify_del_one_attr(s,g,attrname);
#if 0
            es = disable_estack();
            attr_exists = H5Aexists_by_name(g,".",attrname,H5P_DEFAULT);
            restore_estack(es);
            if(attr_exists == FALSE) { 
                dbgf(1,"verify_del_attrs_compact_dense_compact() test: \n");
                dbgf(1,"  attribute %s on group %u is successfully deleted. \n",attrname,which);
                ret = true;
            }
            else if(attr_exists == TRUE) {
                dbgf(1,"verify_del_attrs_compact_dense_compact() test failed \n");
                ret = false;
            }
            else{
                dbgf(1,"H5Aexists_by_name failed \n");
                ret = false;
            }
#endif
        }

        // The writer only deletes the attribute attr-which-0
        HDsprintf(attrname,aname_format,max_c+which,0);
        ret = verify_del_one_attr(s,g,attrname);
        // Add error handling later.
        //
#if 0
        es = disable_estack();
        attr_exists = H5Aexists_by_name(g,".",attrname,H5P_DEFAULT);
        restore_estack(es);
        if(attr_exists == FALSE) { 
            dbgf(1,"verify_del_attrs_compact_dense() test: \n");
            dbgf(1,"  attribute %s on group %u is successfully deleted. \n",attrname,which);
            ret = true;
        }
        else if(attr_exists == TRUE) {
            dbgf(1,"verify_del_attrs_compact_dense() test failed \n");
            ret = false;
        }
        else{
            dbgf(1,"H5Aexists_by_name failed \n");
            ret = false;
        }
#endif
    }
    return ret;

}

static bool
verify_group_attribute(state_t *s, hid_t g, unsigned int which)
{
    char test_pattern = s->at_pattern;
    bool ret = false;
    unsigned max_compact = 0;
    unsigned min_dense = 0;
    hid_t gcpl = H5I_INVALID_HID;
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

static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    //assert(which < s->nsteps);
    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("Number of created groups is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    if(s->use_named_pipes && true == s->attr_test) {
        if(false == np_rd_receive(s)) {
            H5_FAILED(); AT();
            goto error;
        }
        decisleep(s->tick_len * s->update_interval);
        dbgf(1, "reader: finish reading the message: %d\n",s->np_notify);

    }
 
    if((g = H5Gopen(s->file, name, H5P_DEFAULT)) <0) {
        H5_FAILED(); AT();
        printf("H5Gopen failed\n");
        if(s->use_named_pipes && s->attr_test == true) {
            dbgf(1, "Reader: the H5Gopen verfication failed for group %s \n",name);
            np_send_error(s,false);     
        } 
        goto error;
    }

    if(s->use_named_pipes && s->attr_test == true) {
        if(np_rd_send(s)==false) 
            goto error;
        dbgf(1, "Reader: finish sending back the message: %d\n",s->np_notify);
        
    }
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

    return false;

}

// Temp Name pipe works.
#if 0
static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g;
    estack_state_t es;
    bool result = true;
    bool gopen_ret = true;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    if(true == s->attr_test) {
        s->np_verify++;
        /* Receive the notify that the writer bumped up the value */
        if (HDread(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0)
            err(EXIT_FAILURE, "read failed");
        if (s->np_notify != s->np_verify)
            errx(EXIT_FAILURE, "received message %d, expecting %d", s->np_notify, s->np_verify);
        decisleep(3*(s->tick_len));
     dbgf(1, "Reader: finish reading the message: %d\n",s->np_notify);

   }
 
    es = disable_estack();
    g = H5Gopen(s->file, name, H5P_DEFAULT);
    restore_estack(es);

    if (g < 0) 
        gopen_ret = false;
//if(gopen_ret == true) {   
if(s->attr_test == true) {
    if(gopen_ret == false) {
                    dbgf(1, "reader: the gopen verfication failed \n",which);
                    
                    s->np_notify = 0;
    }
                if (HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");

                    dbgf(1, "reader: finish sending back the message: %d\n.", s->np_notify);
    }

if(gopen_ret == true) {
    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(s, g, which);
    else
        result = true;


//}

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");
}
else
   result = false;

    return result;
}

#endif


#if 0
static bool
add_group_attribute(state_t *s, hid_t g, hid_t sid, unsigned int which)
{
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
            H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
        errx(EXIT_FAILURE, "H5Awrite failed");
    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");
}


static bool
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate(, \"%s\", ) failed", name);

    if (s->asteps != 0 && which % s->asteps == 0)
        add_group_attribute(s, g, s->one_by_one_sid, which);

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");
}

static bool
verify_group_attribute(hid_t g, unsigned int which)
{
    estack_state_t es;
    unsigned int read_which;
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which,
        which);

    es = disable_estack();
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        restore_estack(es);
        return false;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_which) < 0) {
        restore_estack(es);
        if (H5Aclose(aid) < 0)
            errx(EXIT_FAILURE, "H5Aclose failed");
        return false;
    }

    restore_estack(es);

    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");

    return read_which == which;
}

static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g;
    estack_state_t es;
    bool result;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    es = disable_estack();
    g = H5Gopen(s->file, name, H5P_DEFAULT);
    restore_estack(es);

    if (g < 0)
        return false;

    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(g, which);
    else
        result = true;

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");

    return result;
}

#endif

//OLDWORK
#if 0
static bool
add_group_attribute(const state_t *s, hid_t g, hid_t sid, unsigned int which)
{
    hid_t aid;
    char  name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
            H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Acreate2 failed\n");
        goto error;
    }

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0) {
        H5_FAILED(); AT();
        printf("H5Awrite failed\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose failed\n");
        goto error;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("group order is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gcreate2 failed\n");
        goto error;
    }

    if (s->asteps != 0 && which % s->asteps == 0)
        result = add_group_attribute(s, g, s->one_by_one_sid, which);

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

    return false;
}

static bool
verify_group_attribute(hid_t g, unsigned int which)
{
    unsigned int read_which;
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which, which);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_which) < 0) {
        H5_FAILED(); AT();
        printf("H5Aread failed\n");
        goto error;
    }

    if (read_which != which) {
        H5_FAILED(); AT();
        printf("H5Aread wrong value\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aread failed\n");
        goto error;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("Group order is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    if ((g = H5Gopen(s->file, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gopen failed\n");
        goto error;
    }

    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(g, which);
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
    unsigned int i;

    if (!state_init(&s, argc, argv)) {
        H5_FAILED(); AT();
        printf("state_init failed\n");
        goto error;
    }

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_group_reader") == 0)
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

    if (writer) {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            if (!write_group(&s, step)) {
                H5_FAILED(); AT();
                printf("write_group failed\n");

                /* At communication interval, notifies the reader about the failture and quit */
                if (s.use_named_pipes && (step % s.csteps == 0)) {
                    notify = -1;
                    HDwrite(fd_writer_to_reader, &notify, sizeof(int));
                }

                goto error;
            } else {
                /* At communication interval, notifies the reader and waits for its response */
                if (s.use_named_pipes && (step % s.csteps == 0)) {
                    /* Bump up the value of notify to notice the reader to start to read */
                    notify++;
                    if (HDwrite(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }

                    /* During the wait, writer makes repeated HDF5 API calls
                     * to trigger EOT at approximately the correct time */
                    for(i = 0; i < config.max_lag + 1; i++) {
                        decisleep(config.tick_len);
                        H5E_BEGIN_TRY {
                            H5Aexists(s.file, "nonexistent");
                        } H5E_END_TRY;
                    }

                    /* Receive the same value from the reader and verify it before
                     * going to the next step */
                    verify++;
                    if (HDread(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDread failed\n");
                        goto error;
                    }

                    if (notify == -1) {
                        H5_FAILED(); AT();
                        printf("reader failed to verify group\n");
                        goto error;
                    }

                    if (notify != verify) {
                        H5_FAILED(); AT();
                        printf("received message %d, expecting %d\n", notify, verify);
                        goto error;
                    }
                }
            }
        }
    }
    else {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "reader: step %d\n", step);

            /* At communication interval, waits for the writer to finish creation before starting verification
             */
            if (s.use_named_pipes && (step % s.csteps == 0)) {
                /* The writer should have bumped up the value of notify.
                 * Do the same with verify and confirm it */
                verify++;

                /* Receive the notify that the writer bumped up the value */
                if (HDread(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
                    H5_FAILED(); AT();
                    printf("HDread failed\n");
                    goto error;
                }

                if (notify == -1) {
                    H5_FAILED(); AT();
                    printf("writer failed to create group\n");
                    goto error;
                }

                if (notify != verify) {
                    H5_FAILED(); AT();
                    printf("received message %d, expecting %d\n", notify, verify);
                    goto error;
                }
            }

            /* Wait for a few ticks for the update to happen */
            if (s.use_named_pipes)
                decisleep(config.tick_len * s.update_interval);

            /* Start to verify group */
            if (!verify_group(&s, step)) {
                H5_FAILED(); AT();
                printf("verify_group failed\n");

                /* At communication interval, tell the writer about the failure and exit */
                if (s.use_named_pipes && (step % s.csteps == 0)) {
                    notify = -1;
                    HDwrite(fd_reader_to_writer, &notify, sizeof(int));
                }

                goto error;
            } else {
                if (s.use_named_pipes && (step % s.csteps == 0)) {
                    /* Send back the same nofity value for acknowledgement to tell the writer
                     * move to the next step */
                    if (HDwrite(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }
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
#endif

int
main(int argc, char **argv)
{

#if 0
    hid_t fapl, fcpl;
    herr_t ret;
    unsigned step;
    bool writer;
    state_t s;
    const char *personality;
    H5F_vfd_swmr_config_t config;
    const char *fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char *fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int fd_writer_to_reader, fd_reader_to_writer;
    // notify = 0 and verify = 0 are for error.
    int notify = 1, verify = 1;
    unsigned int i;

    state_init(&s, argc, argv);

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL &&
        strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL &&
             strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        errx(EXIT_FAILURE,
             "unknown personality, expected vfd_swmr_group_{reader,writer}");
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, FALSE, 128, "./group-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config);

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file == badhid)
        errx(EXIT_FAILURE, writer ? "H5Fcreate" : "H5Fopen");

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (writer) {
        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0)
            errx(EXIT_FAILURE, "HDmkfifo");

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0)
            errx(EXIT_FAILURE, "HDmkfifo");
    }

    /* Both the writer and reader open the pipes */
    if ((fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0)
        errx(EXIT_FAILURE, "fifo_writer_to_reader open failed");

    if ((fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0)
        errx(EXIT_FAILURE, "fifo_reader_to_writer open failed");

#endif

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
    unsigned int i;
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

    if(s.use_named_pipes) {
    s.np_fd_w_to_r = fd_writer_to_reader;
    s.np_fd_r_to_w = fd_reader_to_writer;
    s.np_notify = notify;
    s.np_verify = verify;
    s.tick_len  = config.tick_len;
    s.max_lag   = config.max_lag;
    }

    // TODO: use a different name for s pointer(sp?) in subroutine, 
    // TODO: since its name is also defined as s.

    // For attribute test, force the named pipe to communicate for every step.
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
                if (s.use_named_pipes && s.attr_test !=true && step % s.csteps == 0) {
                //if(1){
                #if 0
                    s.np_notify = -1;
                    HDwrite(fd_writer_to_reader, &(s.np_notify), sizeof(int));
                #endif
                    np_send_error(&s,true);
                }
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
#if 0
                    /* Bump up the value of notify to notice the reader to start to read */
                    s.np_notify++;
                    if (HDwrite(fd_writer_to_reader, &(s.np_notify), sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }

                    /* During the wait, writer makes repeated HDF5 API calls
                     * to trigger EOT at approximately the correct time */
                    for(i = 0; i < config.max_lag + 1; i++) {
                        decisleep(config.tick_len);
                        H5E_BEGIN_TRY {
                            H5Aexists(s.file, "nonexistent");
                        } H5E_END_TRY;
                    }

                    /* Receive the same value from the reader and verify it before
                     * going to the next step */
                    (s.np_verify)++;
                    if (HDread(fd_reader_to_writer, &(s.np_notify), sizeof(int)) < 0){
                        H5_FAILED(); AT();
                        printf("HDread failed\n");
                        goto error;
                    }

                    if (s.np_notify == -1) {
                        H5_FAILED(); AT();
                        printf("reader failed to verify group\n");
                        goto error;
                    }

                    if (s.np_notify != s.np_verify) {
                        H5_FAILED(); AT();
                        printf("received message %d, expecting %d\n", s.np_notify, s.np_verify);
                        goto error;
                    }
#endif
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
#if 0
                /* The writer should have bumped up the value of notify.
                 * Do the same with verify and confirm it */
                s.np_verify++;

                /* Receive the notify that the writer bumped up the value */
                if (HDread(fd_writer_to_reader, &(s.np_notify), sizeof(int)) < 0) {
                    H5_FAILED(); AT();
                    printf("HDread failed\n");
                    goto error;
                }

                if (s.np_notify == -1) {
                    H5_FAILED(); AT();
                    printf("writer failed to create group\n");
                    goto error;
                }

                if (s.np_notify != s.np_verify) {
                    H5_FAILED(); AT();
                    printf("received message %d, expecting %d\n", s.np_notify, s.np_verify);
                    goto error;
                }
#endif
            }

             /* For the default test, wait for a few ticks for the update to happen */
            if(s.use_named_pipes && s.attr_test== false) 
                decisleep(config.tick_len * s.update_interval);

            vg_ret = verify_group(&s, step);

            if (vg_ret == false) {

                printf("verify_group failed\n");
                H5_FAILED(); AT();
                /* At communication interval, tell the writer about the failure and exit */
                if (s.use_named_pipes && s.attr_test != true && step % s.csteps == 0) {
                //if(1){
                    np_send_error(&s,false);
                    //s.np_notify = -1;
                    //HDwrite(fd_reader_to_writer, &(s.np_notify), sizeof(int));
                }
                goto error;

            }
            else {

                /* Send back the same nofity value for acknowledgement to tell the writer
                 * move to the next step. */
               // TO THINK:  reader will never have a chance to acknowledge the writer when attribute verfication occurs.
               // RESOLVED, no need to carry out the following for the attribute operation. It is done in the attribute level.
                if (s.use_named_pipes && s.attr_test!=true && step % s.csteps == 0) {
#if 0
                    if (HDwrite(fd_reader_to_writer, &(s.np_notify), sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }
#endif
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
