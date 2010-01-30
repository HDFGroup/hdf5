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

#define H5F_PACKAGE		/* suppress error about including H5Fpkg   */
#define H5O_PACKAGE             /* suppress error about including H5Opkg   */
#define H5C2_PACKAGE            /* suppress error about including H5C2pkg  */

#define _XOPEN_SOURCE 500	/* for pread() & pwrite() */

#include "hdf5.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "H5private.h"
#include "H5Iprivate.h"
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "H5Fpkg.h"
#include "H5Opkg.h"
#include "H5C2pkg.h"

#define H5F_MAX_SUPERBLOCK_SIZE 134

#define MAX_PATH_LEN	(2 * 1024)

const char *    progname="h5recover";
int             d_status;

/* Define valid command line options. */
static const char *s_opts = "hb:j:fvnVx";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "backup", require_arg, 'b' },
    { "journal", require_arg, 'j' },
    { "force", no_arg, 'f' },
    { "verbose", no_arg, 'v' },
    { "nocopy", no_arg, 'n' },
    { "version", no_arg, 'V' },
    { "examine", no_arg, 'x' },
    { NULL, 0, '\0' }
};


#define MAX_ADDR_SIZE	16
#define MAX_LENGTH_SIZE	16

/* Utility Function Declarations: */

void address_decode(size_t sizeof_addr,
                    const uint8_t **pp /* in, out */,
                    haddr_t *addr_p/*out*/);

void address_encode(size_t sizeof_addr,
                    uint8_t **pp /* in, out */,
                    haddr_t addr);

void length_decode(size_t sizeof_length,
                   const uint8_t **pp /* in, out */,
                   hsize_t *len_p /*out*/);

void length_encode(size_t sizeof_length,
                   uint8_t **pp /* in, out */,
                   hsize_t length);

hbool_t load_buf_from_file(int fd,
                           off_t file_len,
                           off_t offset,
                           size_t buf_len,
                           uint8_t *buf_ptr,
                           unsigned int verbosity,
                           const char * desc,
                           hbool_t * eof_ptr);


/* HDF5 File Investigation Function Declarations: */

hbool_t find_super_block(int fd,
                         off_t file_len,
                         off_t * sb_offset_ptr);

hbool_t get_sb_base_addr(int fd,
                         off_t file_len,
                         off_t sb_offset,
                         int sb_ver,
                         int offset_size,
                         haddr_t * sb_base_addr_ptr);

hbool_t get_journaling_status(char * file_path_ptr,
                              off_t * file_len_ptr,
                              off_t * sb_offset_ptr,
                              int * sb_version_ptr,
                              haddr_t * base_addr_ptr,
                              int * offset_size_ptr,
                              int * length_size_ptr,
                              off_t * eoa_offset_ptr,
                              int32_t * magic_ptr,
                              char * jnl_file_name_ptr,
                              hbool_t * error_ptr,
                              hbool_t examine,
                              unsigned verbosity);

hbool_t get_file_len(char * file_path_ptr,
                     off_t * file_len_ptr);

hbool_t get_mdj_msg_addr(int fd,
                         off_t file_len,
                         int ohdr_ver,
                         int addr_size,
                         int length_size,
                         haddr_t base_addr,
                         haddr_t sb_ext_addr,
                         haddr_t * mdj_msg_addr_ptr,
                         hbool_t examine,
                         unsigned verbosity);

hbool_t get_mdj_msg_addr__scan_chunk(int fd,
                                     off_t file_len,
                                     int ohdr_ver,
                                     int addr_size,
                                     int length_size,
                                     int num_msgs,
                                     int * msg_count_ptr,
                                     int chunk_num,
                                     hbool_t creation_order,
                                     haddr_t base_addr,
                                     haddr_t chunk_addr,
                                     haddr_t chunk_size,
                                     haddr_t * mdj_msg_addr_ptr,
                                     hbool_t examine,
                                     unsigned verbosity);

hbool_t get_mdj_msg_data(int fd,
                         off_t file_len,
                         haddr_t base_addr,
                         haddr_t mdb_msg_addr,
                         hbool_t * enabled_ptr,
                         int32_t * magic_ptr,
                         char * jnl_file_name,
                         hbool_t examine,
                         unsigned verbosity);

hbool_t get_offset_and_length_size(int fd,
                                   off_t file_len,
                                   off_t sb_offset,
                                   int sb_ver,
                                   int * offset_size_ptr,
                                   int * length_size_ptr);

hbool_t get_sb_extension_addr(int fd,
                              off_t file_len,
                              off_t sb_offset,
                              int sb_ver,
                              int offset_size,
                              haddr_t * sb_ext_addr_ptr);

hbool_t get_sb_ext_obj_hdr_ver(int fd,
                               off_t file_len,
                               haddr_t base_addr,
                               haddr_t sb_ext_addr,
                               int * obj_hdr_ver_ptr);

hbool_t get_sb_version(int fd,
                       off_t file_len,
                       off_t sb_offset,
                       int * sb_ver_ptr);


/* HDF5 Journal File Investigation Function Declarations: */

hbool_t get_jnl_header_info(char * file_path_ptr,
                            off_t journal_file_len,
                            size_t * header_length_ptr,
                            int32_t * version_ptr,
                            char * target_file_name_ptr,
                            int32_t * magic_ptr,
                            hbool_t * human_readable_ptr,
                            int * offset_width_ptr,
                            int * length_width_ptr,
                            hbool_t examine,
                            unsigned verbosity);


/* functions supporting the examine option */

void examine_files(char * hdf5_file_name,
                   char * journal_file_name,
		   unsigned verbosity);


/* functions supporting recovery proper */


/* at present, this function must be used with binary journal files only. */
hbool_t apply_journal_file(char * hdf5_file_path_ptr,
                           off_t hdf5_file_len,
                           off_t sb_offset,
                           int sb_ver,
                           haddr_t hdf5_base_addr,
                           char * jnl_file_path_ptr,
                           off_t jnl_file_len,
                           size_t header_length,
                           hbool_t human_readable,
                           int hdf5_offset_width,
                           int hdf5_length_width,
                           uint64_t max_trans_num,
                           haddr_t max_eoa,
                           size_t max_body_len,
                           unsigned verbosity);

hbool_t mark_hdf5_file_recovered(char * hdf5_file_path_ptr);

/* at present, this function must be used with binary journal files only. */
hbool_t scan_journal_file(char * file_path_ptr,
                          off_t file_len,
                          size_t header_length,
                          hbool_t human_readable,
                          int hdf5_offset_width,
                          int hdf5_length_width,
                          uint64_t * max_trans_num_ptr,
                          haddr_t * max_eoa_ptr,
                          size_t * max_body_len_ptr,
                          unsigned verbosity);

hbool_t update_hdf5_eoa(char * hdf5_file_path_ptr,
                        int hdf5_fd,
                        off_t hdf5_file_len,
                        off_t sb_offset,
                        int hdf5_offset_width,
                        int sb_ver,
                        haddr_t base_addr,
                        haddr_t new_eoa,
                        unsigned verbosity);

hbool_t verify_files(char * hdf5_file_name,
                     off_t * hdf5_file_len_ptr,
                     off_t * hdf5_sb_offset_ptr,
                     int * hdf5_sb_ver_ptr,
                     haddr_t * hdf5_base_addr_ptr,
                     char * journal_file_name,
                     off_t * jnl_file_len_ptr,
                     size_t * jnl_header_len_ptr,
                     hbool_t force,
                     hbool_t * human_readable_ptr,
                     int * offset_width_ptr,
                     int * length_width_ptr,
                     unsigned verbosity,
                     hbool_t * error_ptr);


/* binary journal file related #defines, function declarations, and global
 * variable definitions.
 */

#define JF__UNDEFINED_MSG	0
#define JF__BEGIN_TRANS_MSG	1
#define JF__JNL_ENTRY_MSG	2
#define JF__END_TRANS_MSG	3
#define JF__EOA_MSG		4
#define JF__INVALID_MSG		5

hbool_t bjf__chksum_entry_body(int fd,
                               off_t file_len,
                               off_t offset,
                               off_t * new_offset_ptr,
  	                       int * msg_type_ptr,
                               size_t body_length,
                               uint32_t * chksum_ptr,
                               unsigned verbosity);

hbool_t bjf__load_begin_trans_msg(int fd,
                                  off_t file_len,
                                  off_t offset,
                                  off_t * new_offset_ptr,
  		                  int * msg_type_ptr,
                                  int msg_ver,
                                  uint64_t * trans_num_ptr,
                                  unsigned verbosity);

hbool_t bjf__load_chksum(int fd,
                         off_t file_len,
                         off_t offset,
                         off_t * new_offset_ptr,
                         int * msg_type_ptr,
                         uint32_t * chksum_ptr,
                         unsigned verbosity);

hbool_t bjf__load_end_trans_msg(int fd,
                                off_t file_len,
                                off_t offset,
                                off_t * new_offset_ptr,
  	                        int * msg_type_ptr,
                                int msg_ver,
                                uint64_t * trans_num_ptr,
                                unsigned verbosity);

hbool_t bjf__load_entry_body(int fd,
                             off_t file_len,
                             off_t offset,
                             off_t * new_offset_ptr,
  	                     int * msg_type_ptr,
                             size_t body_length,
                             char * buf_ptr,
                             size_t buf_len,
                             uint32_t * chksum_ptr,
                             unsigned verbosity);

hbool_t bjf__load_eoa_msg(int fd,
                          off_t file_len,
                          off_t offset,
                          off_t * new_offset_ptr,
	                  int * msg_type_ptr,
                          int msg_ver,
                          haddr_t * eoa_ptr,
                          int hdf5_offset_width,
                          unsigned verbosity);

hbool_t bjf__load_jnl_entry_msg(int fd,
                                off_t file_len,
                                off_t offset,
                                off_t * new_offset_ptr,
  		                int * msg_type_ptr,
                                int msg_ver,
                                uint64_t * trans_num_ptr,
                                haddr_t * hdf5_offset_ptr,
                                int hdf5_offset_width,
                                size_t * hdf5_length_ptr,
                                int hdf5_length_width,
                                char * buf_ptr,
                                size_t buf_len,
                                unsigned verbosity);

hbool_t bjf__load_length(int fd,
                         off_t file_len,
                         off_t offset,
                         off_t * new_offset_ptr,
                         int * msg_type_ptr,
                         size_t * hdf5_length_ptr,
                         int hdf5_length_width,
                         uint32_t * chksum_ptr,
                         unsigned verbosity);

hbool_t bjf__load_next_msg(int fd,
                           off_t file_len,
                           off_t offset,
                           off_t * new_offset_ptr,
  		           int * msg_type_ptr,
                           uint64_t * trans_num_ptr,
                           haddr_t * hdf5_offset_ptr,
                           int hdf5_offset_width,
                           size_t * hdf5_length_ptr,
                           int hdf5_length_width,
                           haddr_t * eoa_ptr,
                           char * buf_ptr,
                           size_t buf_len,
                           unsigned verbosity);

hbool_t bjf__load_offset(int fd,
                         off_t file_len,
                         off_t offset,
                         off_t * new_offset_ptr,
                         int * msg_type_ptr,
                         haddr_t * hdf5_offset_ptr,
                         int hdf5_offset_width,
                         uint32_t * chksum_ptr,
                         unsigned verbosity);

hbool_t bjf__load_trans_num(int fd,
                            off_t file_len,
                            off_t offset,
                            off_t * new_offset_ptr,
  		            int * msg_type_ptr,
                            uint64_t * trans_num_ptr,
                            uint32_t * chksum_ptr,
                            unsigned verbosity);


/* Unfortunately, H5_checksum_metadata() gives different results depending on
 * how the data is fed to it.  Hence, to duplicate the checksum computed on the
 * library side, we must load the entire metadata journal entry body into RAM
 * and compute the checksum in one call.
 *
 * As we don't know the size of a journal entry body until we read its journal
 * entry message, a great deal of mallocing and freeing would be required if
 * we allocated each buffer as needed, and then freed it when we were done
 * computing the check sum.
 *
 * Instead, we will allocate a global buffer for this purpose, and then increase
 * its size as necessary -- hence the following body_buf and body_buf_len fields.
 */

char * body_buf = NULL;
size_t body_buf_len = 0;


/**************************************************************************/
/********************* HDF5 File Investigation Code ***********************/
/**************************************************************************/
/* The following functions are used to examing the target HDF5 file to    */
/* determine if the supplied journal file can be applied to it, and if so */
/* the offset of the end of file addess field in the superblock.          */
/**************************************************************************/

/**************************************************************************
 *
 * Function:	find_super_block()
 *
 * Purpose:	Scan the supplied file to determine if it is an HDF5
 *		file, and if it is, load the offset into the file of
 *		the superblock in *sb_offset_ptr, and return TRUE.
 *
 *		If the function fails, either because the supplied
 *		file is not an HDF5 file, or for any other reason,
 *		*sb_offset_ptr is undefined, and the function returns
 *		FALSE
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
find_super_block(int fd,
                 off_t file_len,
                 off_t * sb_offset_ptr)
{
    const char * fcn_name = "find_super_block()";
    uint8_t sig_buf[H5F_SIGNATURE_LEN + 1];
    hbool_t found = FALSE;
    hbool_t success = TRUE;
    unsigned int verbosity = 0;
    off_t offset = 0;

    if ( ( fd < 0 ) ||
         ( file_len <= 0 ) ||
         ( sb_offset_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);
    }

    while ( ( success ) && ( ! found ) )
    {
        if ( (offset + H5F_SIGNATURE_LEN) >= file_len ) {

            success = FALSE;

            if ( verbosity > 0 ) {

                HDfprintf(stderr, "%s: Target file is not an HDF5 file.\n");
            }
        } else if ( ! load_buf_from_file(fd, file_len, offset,
                                         (size_t)H5F_SIGNATURE_LEN, sig_buf,
                                         verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 0 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        } else if ( HDmemcmp(sig_buf,
                             H5F_SIGNATURE,
                             (size_t)H5F_SIGNATURE_LEN) == 0 ) {

            found = TRUE;
            *sb_offset_ptr = offset;

        } else if ( offset == 0 ) {

            offset = 512;

        } else {

            offset *= 2;

        }
    }

    return(success);

} /* find_super_block() */


/**************************************************************************
 *
 * Function:	get_sb_base_addr()
 *
 * Purpose:	Read the base address from the super block in the supplied
 *		file using the given superblock offset, load that value
 *		into *sb_base_addr_ptr, and return TRUE.
 *
 *		If the function fails for any other reason,
 *		*sb_base_addr_ptr is undefined, and the function returns
 *		FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
get_sb_base_addr(int fd,
                 off_t file_len,
                 off_t sb_offset,
                 int sb_ver,
                 int offset_size,
                 haddr_t * sb_base_addr_ptr)
{
    const char * fcn_name = "get_sb_base_addr(): ";
    uint8_t addr_buf[MAX_ADDR_SIZE + 1];
    const uint8_t * buf_ptr = addr_buf;
    hbool_t success = TRUE;
    unsigned int verbosity = 0;
    off_t sb_base_addr_offset;
    haddr_t sb_base_addr;

    if ( ( fd < 0 ) ||
         ( (sb_offset % 512) != 0 ) ||
         ( offset_size <= 0 ) ||
         ( sizeof(haddr_t) > (size_t)offset_size ) ||
         ( offset_size > MAX_ADDR_SIZE ) ||
         ( sb_base_addr_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);
    }

    if ( sb_ver != 2 ) {

        success = FALSE;
	HDfprintf(stderr, "%s Unknown SB version (%d).\n", fcn_name, sb_ver);
    }

    if ( success ) {

        sb_base_addr_offset = sb_offset + H5F_SIGNATURE_LEN + 4;

        if ( ! load_buf_from_file(fd, file_len, sb_base_addr_offset,
                                  (size_t)offset_size, addr_buf,
                                  verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 0 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        } else {

            address_decode((size_t)offset_size, &buf_ptr, &sb_base_addr);

            if ( buf_ptr != &(addr_buf[offset_size]) ) {

                success = FALSE;

                if ( verbosity > 0 ) {

                    HDfprintf(stdout,
                            "%s: Unexpected buf_ptr after address_decode().\n",
                             fcn_name);
                }
            } else {

                *sb_base_addr_ptr = sb_base_addr;

            }
        }
    }

    return(success);

} /* get_sb_base_addr() */


/**************************************************************************
 *
 * Function:	get_journaling_status()
 *
 * Purpose:	Determine if the target file exists -- if it doesn't,
 *              issue a diagnostic, and return FALSE.
 *
 *		If it exists, determine if it is an HDF5 file -- if it
 *		isn't, issue a diagnostic, and return FALSE.
 *
 *              If it is an HDF5 file, determine whether it uses a super
 *		block that supports journaling -- if it doesn't, issue a
 *		diagnostic and return FALSE.
 *
 *		If the HDF5 file uses a superblock that supports
 *		journaling, determine whether the file is marked as
 *		having journaling in progress -- if it is not, issue a
 *		diagnostic and return FALSE.
 *
 *		If the target file is an HDF5 file that is marked as
 *		having journaling in progress, determine its:
 *
 *			absolute superblock offset in the file
 *
 *			base address
 *
 *			offset size
 *
 *			length size
 *
 *			absolute offset of the end of file address in
 *			the super block.
 *
 *			magic number assigned to the journal file
 *
 *			name of the journal file
 *
 *		load these values into the locations indicated by the
 *		associated parameters, and return TRUE.
 *
 *		Set error_ptr to TRUE and return FALSE if the function
 *		fails for any reason.
 *
 *		Note that the jnl_file_name_ptr parameter is presumed to
 *		point to a buffer of char of length at least
 *		H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1.
 *
 * Return:	TRUE  if the supplied path references an HDF5 file that
 *		      is marked as having journaling in progress.
 *
 *		FALSE if the supplied path references a file that does
 *		      not exist, doesn't reference an HDF5 file,
 *		      references an HDF5 file that doesn't support
 *		      journaling, references an HDF5 file that supports
 *		      journaling but that is not marked as having
 *		      journaling in progress, or if the function fails
 *		      for any reason.
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	JRM -- 3/11/09
 *		Added the examine parameter.  This doesn't change
 *		the flow of execution, but it does cause the function to
 *		keep up a running commentary to stdout, and to suppress
 *		much of output to stderr that would normally be generated
 *		when errors are detected.
 *
 *		JRM -- 3/19/09
 *		Added the verbosity parameter and associated code.  It
 *		doesn't change the execution, but it does control the
 *		amount of comentary generated.
 *
 **************************************************************************/

hbool_t
get_journaling_status(char * file_path_ptr,
                      off_t * file_len_ptr,
                      off_t * sb_offset_ptr,
                      int * sb_version_ptr,
                      haddr_t * base_addr_ptr,
                      int * offset_size_ptr,
                      int * length_size_ptr,
                      off_t * eoa_offset_ptr,
                      int32_t * magic_ptr,
                      char * jnl_file_name_ptr,
                      hbool_t * error_ptr,
                      hbool_t examine,
                      unsigned verbosity)
{
    const char * fcn_name = "get_journaling_status():";
    const char * indent1 = "";
    const char * indent2 = "\t";
    char jnl_file_name[H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t journaling_enabled = FALSE;
    hbool_t success = TRUE;
    FILE * err_file_ptr = stderr;
    int fd = -1;
    int offset_size;
    int length_size;
    int sb_version;
    int ohdr_version;
    int32_t magic;
    off_t file_len;
    off_t sb_offset;
    off_t eoa_offset;
    haddr_t base_addr;
    haddr_t eoa_addr;
    haddr_t sb_ext_addr;
    haddr_t mdj_msg_addr = HADDR_UNDEF;

    if ( ( file_path_ptr == NULL ) ||
         ( file_len_ptr == NULL ) ||
         ( sb_offset_ptr == NULL ) ||
         ( sb_version_ptr == NULL ) ||
         ( base_addr_ptr == NULL ) ||
         ( offset_size_ptr == NULL ) ||
         ( length_size_ptr == NULL ) ||
         ( eoa_offset_ptr == NULL ) ||
         ( magic_ptr == NULL ) ||
         ( jnl_file_name == NULL ) ||
         ( error_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);
    }

    if ( examine ) {

        HDfprintf(stdout, "Examining the putative HDF5 file %s:\n",
                  file_path_ptr);

        /* We don't want to generate output to stderr when doing an examine
         * unless something is clearly wrong.
         */
        err_file_ptr = stdout;

        /* to make examine output a bit prettier */
	indent1 = "\t";
	indent2 = "\t\t";
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to get the length of %s...\n",
                      indent1, file_path_ptr);
        }

        success = get_file_len(file_path_ptr, &file_len);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(stderr, "Can't stat() %s.  Does the HDF5 file exist?\n",
                      file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sfile length = %lld\n",
                         indent2, (long long)file_len);

            }

            *file_len_ptr = file_len;

            if ( file_len == 0 ) {

                success = FALSE;
                *error_ptr = TRUE;
                HDfprintf(err_file_ptr, "%s%s is empty.\n",
                          indent1, file_path_ptr);
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to open %s...\n",
                      indent1, file_path_ptr);
        }


        /* In the following call, the mode should be ignored. */
        fd = HDopen(file_path_ptr, O_RDONLY, S_IRUSR);

        if ( fd == -1 ) {

            success = FALSE;
            *error_ptr = TRUE;
            HDfprintf(err_file_ptr, "%scan't open %s.\n",
                      indent1, file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sopened file successfully.\n", indent2);
            }
        }
    }


    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%ssearching for super block...\n", indent1);
        }

        success = find_super_block(fd, file_len, &sb_offset);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
		      "%ssuper block not found in %s -- not an HDF5 file?\n",
		      indent1, file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%ssuperblock found at 0x%llx.\n",
                          indent2, (unsigned long long)sb_offset);
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sreading super block version...\n", indent1);
        }

        success = get_sb_version(fd, file_len, sb_offset, &sb_version);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
		     "%scan't get super block verison in %s -- file corrupt?\n",
                     indent1, file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%ssuper block version = %d.\n",
                          indent2, sb_version);
            }

            *sb_version_ptr = sb_version;

            if ( ( sb_version == 0 ) || ( sb_version == 1 ) ) {

                success = FALSE;
                *error_ptr = TRUE;

                HDfprintf(err_file_ptr,
                        "%sjournaling is not supported in the HDF5 file %s.\n",
                        indent1, file_path_ptr);
                HDfprintf(err_file_ptr,
                        "%sthe file may or may not be corrupt, but its super\n",
                        indent1);
                HDfprintf(err_file_ptr,
                        "%sblock doesn't support journaling.\n",
                        indent1);

            } else if ( sb_version != 2 ) {

                success = FALSE;
                *error_ptr = TRUE;

                HDfprintf(err_file_ptr,
                          "%sthe HDF5 file \"%s\" uses an unknown super\n",
                          indent1, file_path_ptr);
                HDfprintf(err_file_ptr,
                          "%sblock version.\n",
                          indent1);
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout,
		      "%sreading offset and length sizes from super block...\n",
                      indent1);
        }

        success = get_offset_and_length_size(fd, file_len, sb_offset,
                                             sb_version, &offset_size,
                                             &length_size);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "%scan't get offset & length sizes from %s.\n",
                      indent1, file_path_ptr);
            HDfprintf(err_file_ptr, "%ssuper block corrupt?\n", indent1);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%soffset_size = %d.\n",
                          indent2, offset_size);
                HDfprintf(stdout, "%slength_size = %d.\n",
                          indent2, length_size);
            }
        }
    }

    if ( success ) { /* make sure offset size is OK */

        if ( sizeof(haddr_t) < (size_t)offset_size ) {

            /* we have a problem -- current definition of haddr_t is too
             * small to represent offsets in the target hdf5 file.  Must
             * abort.
             */

            success = FALSE;
            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "%sOffsets in %s are too big to fit in haddr_t.\n",
                      indent1, file_path_ptr);

            HDfprintf(err_file_ptr, "%ssizeof(haddr_t) = %d.\n",
                      indent2, (int)(sizeof(haddr_t)));

        } else if ( offset_size > MAX_ADDR_SIZE ) {

            success = FALSE;
            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "Offsets in %s are too big for the address buffers in\n",
                      file_path_ptr);

            HDfprintf(err_file_ptr,
                     "this program.  Increase MAX_ADDR_SIZE and recompile.\n");
        }
    }

    if ( success ) { /* make sure length size is OK */

        if ( sizeof(hsize_t) < (size_t)length_size ) {

            /* we have a problem -- current definition of hsize_t is too
             * small to represent object lengths in the target hdf5 file.
             * Must abort.
             */

            success = FALSE;
            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "%sLengths in %s are too big to fit in hsize_t.\n",
                      indent1, file_path_ptr);

            HDfprintf(err_file_ptr, "%ssizeof(hsize_t) = %d.\n",
                      indent2, (int)(sizeof(hsize_t)));

        } else if ( length_size > MAX_LENGTH_SIZE ) {

            success = FALSE;
            *error_ptr = TRUE;

            HDfprintf(stderr,
                      "Lengths in %s are too big for the length buffers in\n",
                      file_path_ptr);

            HDfprintf(stderr,
                   "this program.  Increase MAX_LENGTH_SIZE and recompile.\n");
        }
    }

    if ( success ) { /* compute eoa offset */

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%scomputing eoa addr and offset...\n", indent1);
        }

        switch ( sb_version )
        {
            case 2:
                eoa_addr = (haddr_t)(12 + (2 * offset_size));
                eoa_offset = sb_offset + (off_t)(eoa_addr);

                if ( verbosity > 0 ) {

                    HDfprintf(stdout,
			      "%seoa_addr = 0x%llx\n%seoa_offset = 0x%llx\n",
                              indent2, (long long)eoa_addr,
                              indent2, (long long)eoa_offset);
                }
                break;

            default:
                success = FALSE;
                *error_ptr = TRUE;
                HDfprintf(stderr, "unknown/unsupported SB version.\n");
                HDfprintf(stderr, "This statement should be unreachable.\n");
                break;
        }
    }


    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sreading base addr from super block...\n",
                      indent1);

        }

        success = get_sb_base_addr(fd, file_len, sb_offset, sb_version,
                                   offset_size, &base_addr);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr, "%scan't read base addr from %s.\n",
                      indent1, file_path_ptr);

            HDfprintf(err_file_ptr, "%ssuper block corrupt?\n", indent2);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sbase addr = 0x%llx\n",
                          indent2, (unsigned long long)base_addr);
            }
        }
    }


    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sreading super block extension addr...\n",
                      indent1);
        }

        success = get_sb_extension_addr(fd, file_len, sb_offset, sb_version,
                                        offset_size, &sb_ext_addr);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
	 	      "%scan't read SB extension addr from %s.\n",
                      indent1, file_path_ptr);

            HDfprintf(err_file_ptr, "%ssuper block corrupt?\n", indent2);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%ssuper block extension addr = 0x%llx\n",
                          indent2, (unsigned long long)sb_ext_addr);
            }

            if ( sb_ext_addr == HADDR_UNDEF ) {

                success = FALSE;

                HDfprintf(stdout,
                          "%sThe file %s has no super block extension.\n",
                           indent1, file_path_ptr);
                HDfprintf(stdout, "%s%s %s\n", indent1,
                          "Thus either journaling was not enabled,",
                          "or the file was closed properly.");
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout,
                      "%sreading SB extension object header version...\n",
                      indent1);
        }

        success = get_sb_ext_obj_hdr_ver(fd, file_len, base_addr,
                                         sb_ext_addr, &ohdr_version);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "%scan't read SB extension ohdr ver from %s\n",
                      indent1, file_path_ptr);

            HDfprintf(err_file_ptr, "%sbad address or corrupt ohdr?\n",
                      indent2);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sobject header version = %d.\n",
                          indent2, ohdr_version);
            }

            if ( ( ohdr_version != 1 ) && ( ohdr_version != 2 ) ) {

                success = FALSE;

                *error_ptr = TRUE;

                HDfprintf(err_file_ptr, "%s%s %s\n",
                          indent1,
                          "%sread unknown super block extension",
                          "object header version");
                HDfprintf(err_file_ptr, "%sfrom %s.  %s\n",
                          indent1,
                          file_path_ptr,
                          "Don't know how to find journaling message.");
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout,
		    "%sscanning SB extension for metadata journaling msg...\n",
                    indent1);
        }

        success = get_mdj_msg_addr(fd, file_len, ohdr_version, offset_size,
                                   length_size, base_addr, sb_ext_addr,
                                   &mdj_msg_addr, examine, verbosity);

        if ( ! success ) {

            *error_ptr = TRUE;

            if ( verbosity > 1 ) {

                HDfprintf(stderr, "%s: get_mdj_msg_addr() reports an error.\n",
                          fcn_name);
            }
        } else {

            if ( ( verbosity > 0 ) && ( mdj_msg_addr != HADDR_UNDEF ) ) {

                HDfprintf(stdout,
			  "%smetadata journaling msg addr = 0x%llx\n",
                          indent2, (unsigned long long)mdj_msg_addr);
            }

            if ( mdj_msg_addr == HADDR_UNDEF ) {

		/* the metadata journaling message doesn't exist -- this
                 * implies that metadata journaling is not enabled and that
		 * it is therefore impossible to run a journal.
                 *
                 * Issue a message to this effect and set success to FALSE.
                 *
                 * Note that this is not an error, so we don't set
                 * *error_ptr to TRUE.
                 */

		success = FALSE;

                HDfprintf(stdout,
		          "%sno metadata journaling message found in %s.\n",
                          indent1, file_path_ptr);
                HDfprintf(stdout, "%s%s %s\n", indent1,
                          "Thus either journaling was not enabled,",
                          "or the file was closed properly.");
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sreading metadata journaling message...\n",
                      indent1);
        }

        success =
	    get_mdj_msg_data(fd, file_len, base_addr, mdj_msg_addr,
                             &journaling_enabled, &magic, jnl_file_name,
                             examine, verbosity);

        if ( ! success ) {

            *error_ptr = TRUE;

            HDfprintf(err_file_ptr,
                      "%sCan't read metadata journal msg data from %s.\n",
                      indent1, file_path_ptr);
        }
    }

    if ( fd != -1 ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to close %s...\n",
                      indent1, file_path_ptr);
        }

        if ( HDclose(fd) != 0 ) {

            HDfprintf(stderr, "%s: Error closing %s.  errno = %s (%s).\n",
                      fcn_name, file_path_ptr, errno, HDstrerror(errno));

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sclosed file successfully\n", indent2);
        }
    }

    if ( success ) {

        /* copy relevant data into the locations pointed to by the
         * supplied parameters.
         */
        if ( journaling_enabled ) {

            *sb_offset_ptr    = sb_offset;
            *base_addr_ptr    = base_addr;
            *offset_size_ptr  = offset_size;
            *length_size_ptr  = length_size;
            *eoa_offset_ptr   = eoa_offset;
            *magic_ptr        = magic;
            strcpy(jnl_file_name_ptr, jnl_file_name);

            if ( examine ) {

                HDfprintf(stdout,
                          "%s%s was journaled and not closed properly.\n",
                          indent1, file_path_ptr);
                HDfprintf(stdout, "%s%s %s\n",
                          indent1,
		          "It is probably corrupt, and it cannot be",
                          "opened by the HDF5 library.");
                HDfprintf(stdout, "%sjournaling enabled = %d\n",
                          indent2, (int)journaling_enabled);
                HDfprintf(stdout, "%smagic = 0x%lx\n",
                          indent2, (unsigned long)magic);
                HDfprintf(stdout, "%sjournal file name = \"%s\".\n",
                          indent2, jnl_file_name);
            }
        } else {

            /* nothing to do -- set success to FALSE */
            success = FALSE;

            if ( examine ) {

                HDfprintf(stdout, "%scan't run a journal on %s.\n",
                           indent1, file_path_ptr);
            }
        }
    }

    return(success);

} /* get_journaling_status() */


/**************************************************************************
 *
 * Function:	get_file_len()
 *
 * Purpose:	Stat the supplied file, load its size (in bytes)
 *		in *file_len_ptr, and return TRUE.
 *
 *		If the file does not exist, or if the function fails
 *		for any other reason, *file_len_ptr is undefined, and
 *		the function returns FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 *
 **************************************************************************/

hbool_t
get_file_len(char * file_path_ptr,
             off_t * file_len_ptr)
{
    const char * fcn_name = "get_file_len(): ";
    hbool_t success = TRUE;
    hbool_t verbose = FALSE;
    h5_stat_t buf;

    if ( ( file_path_ptr == NULL ) || ( file_len_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);
    }

    if ( success ) {

        if ( HDstat(file_path_ptr, &buf) != 0 ) {

            success = FALSE;

            if ( verbose ) {

                HDfprintf(stderr, "%s: Can't stat %s.\n",
                          fcn_name, file_path_ptr);
            }
        } else {

            *file_len_ptr = buf.st_size;

        }
    }

    return(success);

} /* get_file_len() */


/**************************************************************************
 *
 * Function:	get_mdj_msg_addr
 *
 * Purpose:	Given the base address of the HDF5 file and the relative
 *		address of the object header containing the super block
 *		extension messages, try to find the address of the metadata
 *		journaling message if it exists.
 *
 *		If it exists, set *mdj_msg_addr_ptr to the relative address
 *		of the message in the file, and return TRUE.
 *
 *		It it does not, set *mdj_msg_addr_ptr to HADDR_UNDEF and
 *		return TRUE.
 *
 *		If any errors are detected, return FALSE.
 *              In this case, *mdj_msg_addr_ptr is undefined.
 *
 * Return:	Success: TRUE if no errors are detected.
 *		Failure: FALSE if any error is detected.
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	JRM -- 3/12/09
 *		Added the examine parameter.  This boolean flag doesn't
 *		change processing, but does cause the function to maintain
 *		a running commentary on its activities.
 *
 *		JRM -- 3/19/09
 *		Added the verbosity parameter and associated code.  It
 *		doesn't change the execution, but it does control the
 *		amount of comentary generated.
 *
 **************************************************************************/

hbool_t
get_mdj_msg_addr(int fd,
                 off_t file_len,
                 int ohdr_ver,
                 int addr_size,
                 int length_size,
                 haddr_t base_addr,
                 haddr_t sb_ext_addr,
                 haddr_t * mdj_msg_addr_ptr,
                 hbool_t examine,
                 unsigned verbosity)
{
    const char * fcn_name = "get_mdj_msg_addr()";
    const char * indent1 = "";
    const char * indent2 = "\t";
    hbool_t creation_order_stored;	/* version 2 header only */
    hbool_t phase_change_values_stored;	/* version 2 header only */
    hbool_t times_stored;		/* version 2 header only */
    hbool_t success = TRUE;
    uint8_t buf[64];
    const uint8_t * p = buf;
    uint8_t flags;
    int msg_count = 0;
    uint16_t num_hdr_msgs;		/* version 1 header only */
    uint32_t junk;
    uint32_t ohdr_size;			/* version 1 header only */
    uint64_t chunk_0_size;
    int size_of_chunk_0_size;		/* version 2 header only */
    haddr_t chunk_0_addr;
    haddr_t mdj_msg_addr = HADDR_UNDEF;
    off_t offset;
    FILE * err_file_ptr = stderr;

    if ( ( fd < 0 )  ||
         ( ( ohdr_ver != 1 ) && ( ohdr_ver != 2 ) ) ||
         ( mdj_msg_addr_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);
    }

    if ( examine ) {

        /* We don't want to generate output to stderr when doing an examine
         * unless something is clearly wrong.
         */
        err_file_ptr = stdout;

        /* to make examine output a bit prettier */
	indent1 = "\t";
	indent2 = "\t\t";
    }


    switch ( ohdr_ver ) {

        case 1:

            if ( success ) {

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%spreparing to examine v1 SBE ohdr...\n",
                              indent1);
                }

                /* set the offset to point to the number of messages */
                offset = (off_t)(base_addr + sb_ext_addr + 2);

                success = load_buf_from_file(fd, file_len, offset, (size_t)10,
                                             buf, verbosity, fcn_name, NULL);

                if ( ! success ) {

                    HDfprintf(err_file_ptr,
		      "%scan't load section of ohdr -- file corruption?\n",
                      indent1);
                }
            }

            if ( success ) {

                p = buf;
                UINT16DECODE(p, num_hdr_msgs);
                UINT32DECODE(p, junk);
                UINT32DECODE(p, ohdr_size);

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%snum hdr msgs = %d.\n",
			      indent2, (int)num_hdr_msgs);
		    HDfprintf(stdout, "%schunk 0 base addr = 0x%llx.\n",
                              indent2, (unsigned long long)(sb_ext_addr + 12));
		    HDfprintf(stdout, "%schunk 0 length = 0x%llx.\n",
                              indent2, (unsigned long long)ohdr_size);
                }
            }

            if ( success ) {

                success = get_mdj_msg_addr__scan_chunk(fd,
                                                   file_len,
                                                   ohdr_ver,
                                                   addr_size,
                                                   length_size,
                                                   num_hdr_msgs,
                                                   &msg_count,
                                                   0,
                                                   FALSE,
                                                   base_addr,
                                                   (haddr_t)(sb_ext_addr + 12),
                                                   (haddr_t)ohdr_size,
                                                   &mdj_msg_addr,
                                                   examine,
                                                   verbosity);
            }
            break;

        case 2:

            if ( ( success ) && ( verbosity > 0 ) ) {

                HDfprintf(stdout, "%spreparing to examine v2 SBE ohdr...\n",
                          indent1);
            }

            if ( success ) { /* read the object header flags */

                offset = (off_t)(base_addr + sb_ext_addr + 5);

                success = load_buf_from_file(fd, file_len, offset, (size_t)1,
                                             &flags, verbosity, fcn_name,
                                             NULL);
            }

            if ( success ) { /* decode the flags we care about */

                switch ( flags & 0x3 )
                {
                    case 0:
                        size_of_chunk_0_size = 1;
                        break;

                    case 1:
                        size_of_chunk_0_size = 2;
                        break;

                    case 2:
                        size_of_chunk_0_size = 4;
                        break;

                    case 3:
                        size_of_chunk_0_size = 8;
                        break;

                    default:
                        /* this case is unreachable -- included to keep the
                         * compiler happy.
                         */
                       HDfprintf(stderr, "%s: reached the unreachable.\n",
                                 fcn_name);
                       break;
                }

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%ssize of chunk size = %d\n",
                              indent2, size_of_chunk_0_size);
                }

                if ( (flags & 0x4) != 0 ) {

                    creation_order_stored = TRUE;

                } else {

                    creation_order_stored = FALSE;

                }

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%screation order stored = %d\n",
                              indent2, (int)creation_order_stored);
                }

                if ( (flags & 0x10) != 0 ) {

		    phase_change_values_stored = TRUE;

                } else {

		    phase_change_values_stored = FALSE;

                }

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%sphase change values stored = %d\n",
                              indent2, (int)phase_change_values_stored);
                }

                if ( (flags & 0x20) != 0 ) {

		    times_stored = TRUE;

		} else {

		    times_stored = FALSE;

		}

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%stimes stored = %d\n",
                              indent2, (int)times_stored);
                }
            }

            if ( success ) { /* get the chunk 0 size */

                offset = (off_t)(base_addr + sb_ext_addr + 6);

                if ( times_stored ) {

		    offset += (off_t)16;
		}

                if ( phase_change_values_stored ) {

		    offset += (off_t)4;
                }

                success = load_buf_from_file(fd, file_len, offset,
                                             (size_t)size_of_chunk_0_size,
                                              buf, verbosity, fcn_name, NULL);
            }

            if ( success ) {

		p = buf;
                chunk_0_size = 0;

                switch ( size_of_chunk_0_size )
                {
                    case 1:
                        chunk_0_size = (uint64_t)(*p);
                        HDassert( chunk_0_size < 256 );
                        break;

                    case 2:
                        {
                            uint16_t tmp;
                            UINT16DECODE(p, tmp);
                            chunk_0_size = (uint64_t)tmp;
                            HDassert( chunk_0_size < 65536 );
                        }
                        break;

                    case 4:
                        {
                            uint32_t tmp;
                            UINT32DECODE(p, tmp);
                            chunk_0_size = (uint64_t)tmp;
                            HDassert( chunk_0_size <= 4294967295UL );
                        }
                        break;

                    case 8:
                        UINT64DECODE(p, chunk_0_size);
                        break;

                    default:
                        success = FALSE;
                        HDfprintf(stderr, "%s: unknown chunk 0 size (%d).\n",
                                  fcn_name, size_of_chunk_0_size);
                        break;
                }

                if ( verbosity > 0 ) {

                    HDfprintf(stdout, "%schunk 0 size = 0x%llx\n",
                              indent2, (unsigned long long)chunk_0_size);
                }
            }

            if ( success ) {

                if ( size_of_chunk_0_size > addr_size ) {

                    success = FALSE;
                    HDfprintf(err_file_ptr,
                              "%s%s %s\n",
                              indent1,
                              "size of chunk 0 size larger than file"
                              "address size!?!");
                    HDfprintf(err_file_ptr,
                              "%ssize_of_chunk_0_size = %d > addr_size = %d.\n",
                              indent1, size_of_chunk_0_size, addr_size);

                } else {

                    chunk_0_addr = (haddr_t)(sb_ext_addr + 6);

                    if ( times_stored ) {

		        chunk_0_addr += (haddr_t)16;
		    }

                    if ( phase_change_values_stored ) {

		        chunk_0_addr += (haddr_t)4;
                    }

                    chunk_0_addr += (haddr_t)size_of_chunk_0_size;


                    if ( verbosity > 0 ) {

                        HDfprintf(stdout, "%schunk 0 addr = 0x%llx\n",
                                  indent2, (unsigned long long)chunk_0_addr);
                    }
                }
            }

            if ( success ) {

                success = get_mdj_msg_addr__scan_chunk(fd,
                                                       file_len,
                                                       ohdr_ver,
                                                       addr_size,
                                                       length_size,
                                                       num_hdr_msgs,
                                                       &msg_count,
                                                       0,
                                                       creation_order_stored,
                                                       base_addr,
                                                       chunk_0_addr,
                                                       (haddr_t)chunk_0_size,
                                                       &mdj_msg_addr,
                                                       examine,
                                                       verbosity);
            }
            break;

        default:
	    HDfprintf(stderr, "%s: ohdr_ver = %d out of range.\n",
                      fcn_name, ohdr_ver);
            break;
    }

    if ( ( success ) && ( mdj_msg_addr != HADDR_UNDEF ) ) {

        *mdj_msg_addr_ptr = mdj_msg_addr;
    }

    return(success);

} /* get_mdj_msg_addr() */


/**************************************************************************
 *
 * Function:	get_mdj_msg_addr__scan_chunk()
 *
 * Purpose:	Given the address of a chunk of messages associated with
 *		the object header containing the super block extension
 *		of the supplied file, scan the chunk for a metadata
 *		journaling message, and return its address in
 *		*mdj_msg_addr_ptr if one is found.
 *
 *		*mdj_msg_addr_ptr is undefined if no metadata journaling
 *		message is found.
 *
 *		If any errors are detected, return FALSE.
 *              In this case, *mdj_msg_addr_ptr is undefined.
 *
 * Return:	Success: TRUE if no errors are detected.
 *		Failure: FALSE if any error is detected.
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	JRM -- 3/12/09
 *		Added the examine parameter.  This boolean flag doesn't
 *		change processing, but does cause the function to maintain
 *		a running commentary on its activities.
 *
 *		JRM -- 3/19/09
 *		Added the verbosity parameter and associated code.  It
 *		doesn't change the execution, but it does control the
 *		amount of comentary generated.
 *
 **************************************************************************/

hbool_t
get_mdj_msg_addr__scan_chunk(int fd,
                             off_t file_len,
                             int ohdr_ver,
                             int addr_size,
                             int length_size,
                             int num_msgs,  /* ohdr version 1 only */
                             int * msg_count_ptr,  /* ohdr version 1 only */
                             int chunk_num,
                             hbool_t creation_order, /* ohdr version 2 only */
                             haddr_t base_addr,
                             haddr_t chunk_addr,
                             haddr_t chunk_size,
                             haddr_t * mdj_msg_addr_ptr,
                             hbool_t examine,
                             unsigned verbosity)
{
    const char * fcn_name = "get_mdj_msg_addr__scan_chunk()";
    const char * indent1 = "";
    const char * indent2 = "\t";
    uint8_t flags;
    uint8_t buf[64];
    const uint8_t * p = buf;
    uint16_t num_hdr_msgs; /* version 1 header only */
    uint16_t msg_type_num;
    uint16_t msg_data_size;
    int null_msg_count = 0;
    hbool_t success = TRUE;
    haddr_t cum_msg_size = 0;
    haddr_t signature_size = 0; /* version 2 header only */
    haddr_t msg_hdr_size;
    haddr_t msg_addr;
    haddr_t msg_data_addr;
    haddr_t new_chunk_addr;
    hsize_t new_chunk_len;
    off_t offset;
    FILE * err_file_ptr = stderr;

    /* make sure the buffer is big enough */
    HDassert( (MAX_ADDR_SIZE + MAX_LENGTH_SIZE) <= 64 );

    if ( ( fd < 0 )  ||
         ( ( ohdr_ver != 1 ) && ( ohdr_ver != 2 ) ) ||
         ( mdj_msg_addr_ptr == NULL ) ||
         ( *mdj_msg_addr_ptr != HADDR_UNDEF ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);
    }

    if ( examine ) {

        /* We don't want to generate output to stderr when doing an examine
         * unless something is clearly wrong.
         */
        err_file_ptr = stdout;

        /* to make examine output a bit prettier */
	indent1 = "\t";
	indent2 = "\t\t";
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sscanning SB ext chunk %d.\n",
                      indent1, chunk_num);
        }

        switch ( ohdr_ver )
        {
            case 1:
                msg_hdr_size = 8;
                break;

            case 2:
                if ( creation_order ) {

                    msg_hdr_size = 5;

                } else {

                    msg_hdr_size = 4;

                }

                if ( chunk_num != 0 ) {
                    /* continuation chunk -- check signature */

                    signature_size = H5O_SIZEOF_MAGIC;

                    offset = (off_t)(base_addr + chunk_addr);

                    success = load_buf_from_file(fd, file_len, offset,
                                                 (size_t)H5O_SIZEOF_MAGIC,
                                                 buf, verbosity, fcn_name,
                                                 NULL);

                    if ( success ) {

                        if ( HDmemcmp(buf,
                                      H5O_CHK_MAGIC,
                                      (size_t)H5O_SIZEOF_MAGIC) != 0 ) {

                            success = FALSE;

                            HDfprintf(err_file_ptr, "%s%s %d %s %s\n",
                                       indent1,
                                       "SB extension continuation chunk",
                                       chunk_num,
                                       "has bad signature --",
                                       "file corruption?");
                        } else if ( verbosity > 1 ) {

                            HDfprintf(stdout,
                                      "%sContinuation message signature OK.\n",
                                      indent2);
                        }
                    }
                }
                break;

            default: /* just keeping the compiler happy -- this can't happen */
                success = FALSE;
                HDfprintf(stderr, "%s: reached the unreachable.\n", fcn_name);
                break;
        }
    }

    /* scan the messages */
    while ( ( success ) &&
            ( cum_msg_size < chunk_size ) ) {

        msg_addr = chunk_addr + signature_size + cum_msg_size;

        if ( verbosity > 1 ) {

            HDfprintf(stdout, "%schunk_size = 0x%llx\n",
                      indent2, (unsigned long long)chunk_size);
            HDfprintf(stdout, "%scum_msg_size = 0x%llx\n",
                      indent2, (unsigned long long)cum_msg_size);
        }

        /* set the offset to point to the header of the next message */
        offset = (off_t)(base_addr + msg_addr);

        /* load the message type, size, flags, and possibly creation order */
        success = load_buf_from_file(fd, file_len, offset,
                                     (size_t)msg_hdr_size,
                                     buf, verbosity, fcn_name, NULL);

        if ( success ) {

            p = buf;

            switch ( ohdr_ver )
            {
                case 1:
                    UINT16DECODE(p, msg_type_num);
                    UINT16DECODE(p, msg_data_size);
                    flags = (uint8_t)(*p);
                    break;

                case 2:
                    msg_type_num = (uint16_t)(*p++);
                    UINT16DECODE(p, msg_data_size);
                    flags = (uint8_t)(*p);
                    break;

                default: /* keeping the compiler happy -- this can't happen */
                    success = FALSE;
                    HDfprintf(stderr, "%s: reached the unreachable.\n",
                    fcn_name);
                    break;
            }
        }

        if ( ( success ) && ( verbosity > 1 ) ) {

            HDfprintf(stdout,
                      "%smsg: %d  type = %d  dsize = %d flags = 0x%x\n",
                      indent2, *msg_count_ptr, (int)msg_type_num,
                      (int)msg_data_size, (unsigned)flags);
        }

        msg_data_addr = msg_addr + msg_hdr_size;

        if ( success ) {

            switch ( msg_type_num )
            {

                case H5O_MDJ_MSG_ID:
                    if ( verbosity > 0 ) {

                        HDfprintf(stdout,
				  "%sfound journaling message at 0x%llx\n",
                                  indent1,
                                  (unsigned long long)msg_data_addr);
                    }

                    if ( *mdj_msg_addr_ptr == HADDR_UNDEF ) {

                        *mdj_msg_addr_ptr = msg_data_addr;

                    } else {

                        /* found a duplicate metadata journaling
                         * message -- scream and die.
                         */

                        success = FALSE;

                        HDfprintf(err_file_ptr, "%s%s %s",
                                  indent1,
                                  "Found duplicate metadata jouraling",
                                  "message in super block extension.\n");
                        HDfprintf(err_file_ptr, "%s%s %s",
                                  indent1,
                                  "Probably a bug in the library. ",
                                  "Exiting without action.\n");
                    }
                    break;

                case H5O_NULL_ID:
                    null_msg_count++;
                    break;

                case H5O_SHMESG_ID:
                case H5O_BTREEK_ID:
		case H5O_DRVINFO_ID:
                    /* These messages are allowed in superblock
		     * extension messages -- just skip over them.
                     */
		    break;

                case H5O_CONT_ID:
                    /* read the message and then scan the chunk it
                     * points to.
                     */
                    if ( verbosity > 0 ) {

                        HDfprintf(stdout,
                                 "%dfound continuation msg -- chunk %d\n",
                                 indent1,
                                 chunk_num + 1);
                    }

                    success = load_buf_from_file(fd, file_len, offset,
                                         (size_t)(addr_size + length_size + 1),
                                         buf, verbosity, fcn_name, NULL);

                    if ( success ) {

                        address_decode((size_t)addr_size, &p, &new_chunk_addr);

                        if ( p != &(buf[addr_size]) ) {

                            success = FALSE;

                            if ( verbosity > 2 ) {

                                HDfprintf(stdout, "%s: %s %s\n",
                                          fcn_name,
                                          "Unexpected p after",
                                          "address_decode().\n");
                            }
                        }
                    }

                    if ( success ) {

                        length_decode((size_t)length_size, &p, &new_chunk_len);

                        if ( p != &(buf[addr_size + length_size]) ) {

                            success = FALSE;

                            if ( verbosity > 2 ) {

                                HDfprintf(stdout, "%s: %s %s\n",
                                          fcn_name,
                                          "Unexpected p after",
                                          "length_decode().\n");
                            }
                        }
                    }

                    if ( ( success ) && ( verbosity > 0 ) ) {

                        HDfprintf(stdout, "%schunk %d addr = 0x%llx\n",
                                  indent2,
                                  chunk_num + 1,
                                  (unsigned long long)new_chunk_addr);
                        HDfprintf(stdout, "%schunk %d length = 0x%llx\n",
                                  indent2,
                                  chunk_num + 1,
                                  (unsigned long long)new_chunk_len);
                    }

                    if ( success ) {

                        success = get_mdj_msg_addr__scan_chunk
                                  (
                                      fd,
                                      file_len,
                                      ohdr_ver,
                                      addr_size,
                                      length_size,
                                      num_msgs,
                                      msg_count_ptr,
                                      chunk_num + 1,
                                      creation_order,
                                      base_addr,
                                      new_chunk_addr,
                                      new_chunk_len,
                                      mdj_msg_addr_ptr,
                                      examine,
                                      verbosity
                                  );
                    }
                    break;

                case H5O_SDSPACE_ID:
                case H5O_LINFO_ID:
                case H5O_DTYPE_ID:
                case H5O_FILL_ID:
                case H5O_FILL_NEW_ID:
                case H5O_LINK_ID:
                case H5O_EFL_ID:
                case H5O_LAYOUT_ID:
                case H5O_BOGUS_ID:
                case H5O_GINFO_ID:
                case H5O_PLINE_ID:
                case H5O_ATTR_ID:
                case H5O_NAME_ID:
                case H5O_MTIME_ID:
                case H5O_STAB_ID:
                case H5O_MTIME_NEW_ID:
                case H5O_AINFO_ID:
                case H5O_REFCOUNT_ID:
                    /* these messages clearly shouldn't be in
                     * the superblock extension -- scream and die.
                     */
                    success = FALSE;

                    HDfprintf(err_file_ptr,
                            "%sSB ext chunk %d contains a dis-allowed mssg.\n",
                            indent1, chunk_num);
                    HDfprintf(stdout, "%sprobably a bug in library\n", indent1);
                    break;

                default:
                    /* Unknown message -- scream, but don't die. */
                    HDfprintf(err_file_ptr, "%sSB ext chunk %d %s\n",
                              indent1, chunk_num,
                              "appears to contain an unknown mssg.");
                    break;
            }

            cum_msg_size += (haddr_t)(msg_hdr_size + msg_data_size);

            (*msg_count_ptr)++;
        }

        /* no gaps possible in version 1, but must check for version 2 */
        if ( ( success ) && ( ohdr_ver == 2 ) ) {

            if ( ( cum_msg_size < chunk_size ) &&
                 ( (chunk_size - cum_msg_size) <= msg_hdr_size ) ) {

                if ( null_msg_count > 0 ) {

                    success = FALSE;

                    HDfprintf(err_file_ptr,
			      "%sfound gap in SB ext chunk with null msg.\n",
                              indent1);
                    HDfprintf(err_file_ptr, "%slibrary bug?.\n", indent1);

                }
                cum_msg_size = chunk_size;
            }
        }

        /* check for too many messages -- version 1 ohdr only */
        if ( ( success ) &&
             ( ohdr_ver == 1 ) &&
             ( *msg_count_ptr > num_hdr_msgs ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%sSB extension has too many messages.\n",
                      indent1);
            HDfprintf(err_file_ptr, "%slibrary bug?.\n", indent1);

        }
    } /* end while */

    if ( verbosity > 2 ) {

        HDfprintf(stdout, "%s: exiting while loop.\n", fcn_name);
        HDfprintf(stdout, "%s: chunk_size = 0x%llx\n", fcn_name,
                  (unsigned long long)chunk_size);
        HDfprintf(stdout, "%s: cum_msg_size = 0x%llx\n", fcn_name,
                  (unsigned long long)cum_msg_size);
    }


    return(success);

} /* get_mdj_msg_addr__scan_chunk() */


/**************************************************************************
 *
 * Function:	get_mdj_msg_data
 *
 * Purpose:	Given the base address of the HDF5 file and the relative
 *		address of the metadata journaling super block extension
 *		message, read the message, copy its contents into the
 *		locations indicated by the enabled_ptr, magic_ptr, and
 *		jnl_file_name parameters, and return TRUE.
 *
 *		Note that only * enabled_ptr is defined if journaling is
 *		not enabled.
 *
 *		Note also that jnl_file_name is presumed to point to a
 *		buffer of char of length at least
 *		H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1.
 *
 *		If any errors are detected, return FALSE.
 *              In this case, *enabled_ptr, *magic_ptr and the
 *		contents of jnl_file_name are undefined.
 *
 * Return:	Success: TRUE if no errors are detected.
 *		Failure: FALSE if any error is detected.
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	JRM -- 3/12/09
 *		Added the examine parameter.  This boolean flag doesn't
 *		change processing, but does cause the function to maintain
 *		a running commentary on its activities.
 *
 *		JRM -- 3/19/09
 *		Added the verbosity parameter and associated code.  It
 *		doesn't change the execution, but it does control the
 *		amount of comentary generated.
 *
 **************************************************************************/

#define MDJ_MSG_HDR_LEN	( 1 + 	/* version number */			\
			  2 +	/* flags */				\
			  4 +	/* magic -- sizeof(int32_t) */		\
			  4 )	/* jnl file path len -- sizeof(int32_t) */

#define MAX_MDJ_MSG_LEN	(MDJ_MSG_HDR_LEN + H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1)

hbool_t
get_mdj_msg_data(int fd,
                 off_t file_len,
                 haddr_t base_addr,
                 haddr_t mdj_msg_addr,
                 hbool_t * enabled_ptr,
                 int32_t * magic_ptr,
                 char * jnl_file_name,
                 hbool_t examine,
                 unsigned verbosity)
{
    const char * fcn_name = "get_mdj_msg_data()";
    const char * indent1 = "";
    const char * indent2 = "\t";
    hbool_t journaling_enabled = FALSE;
    hbool_t success = TRUE;
    uint8_t buf[MAX_MDJ_MSG_LEN + 1];
    uint8_t * p;
    uint16_t flags;
    int32_t magic;
    uint32_t path_len;
    int version;
    off_t offset;
    FILE * err_file_ptr = stderr;

    if ( ( fd < 0 ) ||
         ( enabled_ptr == NULL ) ||
         ( magic_ptr == NULL ) ||
         ( jnl_file_name == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);

    }

    if ( examine ) {

        /* We don't want to generate output to stderr when doing an examine
         * unless something is clearly wrong.
         */
        err_file_ptr = stdout;

        /* to make examine output a bit prettier */
	indent1 = "\t";
	indent2 = "\t\t";
    }


    if ( ( success ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout, "%sreading metadata journaling message.\n", indent1);
    }

    if ( success ) {

        offset = (off_t)(base_addr + mdj_msg_addr);

        success = load_buf_from_file(fd, file_len, offset,
                                     (size_t)MDJ_MSG_HDR_LEN,
                                     buf, verbosity, fcn_name, NULL);
    }

    if ( success ) {

        version = (int)(buf[0]);

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sMDJ msg version = %d\n", indent2, version);
        }

        /* Only know about one version -- scream and die if unknown version */
        if ( version != H5O_MDJ_CONF_VERSION ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%s%s %s (%d) %s\n",
                      indent1,
                      "found metadata journaling message with",
                      "unkown version",
                      version,
                      "-- file corruption?");
        }
    }

    if ( success ) {

        p = &(buf[1]);
        UINT16DECODE(p, flags);
        INT32DECODE(p, magic);
        UINT32DECODE(p, path_len);

        if ( (flags & MDJ_MSG__JOURNALING_ENABLED_FLAG) == 0 ) {

            /* journaling is disabled -- this case shouldn't occur so we
             * we will print a warning.
             */
            journaling_enabled = FALSE;

            HDfprintf(err_file_ptr,
                      "%s%s %s\n",
                      indent1,
                      "Found a metadata journaling message saying that",
            	      "journaling is disabled.");

        } else {

            journaling_enabled = TRUE;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sjournaling enabled = TRUE\n", indent2);
                HDfprintf(stdout, "%smagic = 0x%x\n", indent2, (unsigned)magic);
                HDfprintf(stdout, "%sjournal file path len = %d\n",
                          indent2, (int)path_len);
            }

            if ( ( path_len <= 0 ) ||
                 ( path_len > H5C2__MAX_JOURNAL_FILE_NAME_LEN ) ) {

                success = FALSE;

                HDfprintf(err_file_ptr, "%s%s %s (%d).\n",
                          indent1,
                          "Encountered metadata journaling message with",
                          "path len our of range",
                          path_len);
            }
        }
    }

    if ( ( success ) && ( journaling_enabled ) ) {

        offset = (off_t)(base_addr + mdj_msg_addr + MDJ_MSG_HDR_LEN);

        success = load_buf_from_file(fd, file_len, offset,
                                     (size_t)(path_len + 1),
                                     buf, verbosity, fcn_name, NULL);
    }

    if ( ( success ) && ( journaling_enabled ) ) {

        if ( strlen((char *)buf) != (size_t)path_len ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "Encountered metadata journaling message with\n");
            HDfprintf(err_file_ptr,
                      "path_len not matching actual path len.\n");
        }
    }

    if ( success ) {

        if ( journaling_enabled ) {

            *enabled_ptr = TRUE;
            *magic_ptr = magic;
            strcpy(jnl_file_name, (char *)buf);

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "	journal file = \"%s\"\n", buf);
            }
        } else {

            *enabled_ptr = FALSE;

        }
    }

    return(success);

} /* get_mdj_msg_data() */


/**************************************************************************
 *
 * Function:	get_offset_and_length_size
 *
 * Purpose:	Obtain the number of bytes required to represent offsets
 *		and lengths in the target HDF5 file, load these values into
 *		*offset_size_ptr and *length_size_ptr, and return TRUE.
 *
 *		If the function fails for any other reason,
 *		*offset_size_ptr is undefined, and the function
 *		returns FALSE.
 *
 *		Note that at present, this function works only with
 *		the HDF5 version 2 super block, as that is the only
 *		version (for now) that supports journaling.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
get_offset_and_length_size(int fd,
                           off_t file_len,
                           off_t sb_offset,
                           int sb_ver,
                           int * offset_size_ptr,
                           int * length_size_ptr)
{
    const char * fcn_name = "get_offset_and_length_size()";
    uint8_t buf[2];
    hbool_t success = TRUE;
    off_t offset;
    unsigned int verbosity = 0;
    int offset_size;
    int length_size;

    if ( ( fd < 0 ) ||
         ( (sb_offset % 512) != 0 ) ||
         ( offset_size_ptr == NULL ) ||
         ( length_size_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);

    } else if ( sb_ver != 2 ) {

        success = FALSE;
	HDfprintf(stderr, "%s: Unsupported superblock version (%d).\n",
                  fcn_name, sb_ver);
    }

    if ( success ) {

        offset = sb_offset + H5F_SIGNATURE_LEN + 1;

        if ( ! load_buf_from_file(fd, file_len, offset, (size_t)2,
                                  buf, verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        } else {

            offset_size = (int)buf[0];
            length_size = (int)buf[1];

        }
    }

    if ( success ) {

        if ( offset_size <= 0 ) {

            success = FALSE;

            if ( verbosity > 2 ) {
                HDfprintf(stderr, "%s found non-positive offset size: %d\n",
                          fcn_name, offset_size);

            }
        } else {

            *offset_size_ptr = offset_size;

        }
    }

    if ( success ) {

        if ( length_size <= 0 ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr, "%s found non-positive length size: %d\n",
                          fcn_name, length_size);

            }
        } else {

            *length_size_ptr = length_size;

        }
    }

    return(success);

} /* get_offset_and_length_size() */


/**************************************************************************
 *
 * Function:	get_sb_extension_addr()
 *
 * Purpose:	Read the super block extension address from the supplied
 *		file using the given superblock offset, load that value
 *		into *sb_ext_addr_ptr, and return TRUE.
 *
 *		If the function fails for any other reason,
 *		*sb_ext_addr_ptr is undefined, and the function returns
 *		FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
get_sb_extension_addr(int fd,
                      off_t file_len,
                      off_t sb_offset,
                      int sb_ver,
                      int offset_size,
                      haddr_t * sb_ext_addr_ptr)
{
    const char * fcn_name = "get_sb_extension_addr(): ";
    uint8_t addr_buf[MAX_ADDR_SIZE + 1];
    const uint8_t * buf_ptr = addr_buf;
    hbool_t success = TRUE;
    unsigned int verbosity = 0;
    off_t sb_ext_offset;
    haddr_t sb_ext_addr;

    if ( ( fd < 0 ) ||
         ( (sb_offset % 512) != 0 ) ||
         ( offset_size <= 0 ) ||
         ( sizeof(haddr_t) > (size_t)offset_size ) ||
         ( offset_size > MAX_ADDR_SIZE ) ||
         ( sb_ext_addr_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);
    }

    if ( sb_ver != 2 ) {

        success = FALSE;
	HDfprintf(stderr, "%s Unknown SB version (%d).\n", fcn_name, sb_ver);
    }

    if ( success ) {

        sb_ext_offset = sb_offset + H5F_SIGNATURE_LEN + 4 + offset_size;

        if ( ! load_buf_from_file(fd, file_len, sb_ext_offset,
                                  (size_t)offset_size, addr_buf,
                                  verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        } else {

            address_decode(sizeof(haddr_t), &buf_ptr, &sb_ext_addr);

	    if ( buf_ptr != &(addr_buf[offset_size]) ) {

                success = FALSE;

                if ( verbosity > 2 ) {

                    HDfprintf(stdout,
                            "%s: Unexpected buf_ptr after address_decode().\n",
                            fcn_name);
                }
            } else {

                *sb_ext_addr_ptr = sb_ext_addr;

            }
        }
    }

    return(success);

} /* get_sb_extension_addr() */


/**************************************************************************
 *
 * Function:	get_sb_ext_obj_hdr_ver()
 *
 * Purpose:	Given the supplied HDF5 file base address and super block
 *		extension relative address, attempt to determin the
 *		version of object header being used to store the super
 *		block extension messages, and return this value in
 *		*obj_hdr_type_ptr.
 *
 *		If successful, return TRUE.  On failure for any reason,
 *		return FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
get_sb_ext_obj_hdr_ver(int fd,
                       off_t file_len,
                       haddr_t base_addr,
                       haddr_t sb_ext_addr,
                       int * ohdr_ver_ptr)
{
    const char * fcn_name = "get_sb_ext_obj_hdr_ver()";
    /* the buffer must be large enough for the object header magic and
     * its version -- note that version 1 doesn't have magic.
     */
    uint8_t buf[H5O_SIZEOF_MAGIC + 2];
    hbool_t success = TRUE;
    unsigned int verbosity = 0;
    int ohdr_ver;
    off_t ohdr_offset;

    if ( ( fd < 0 ) ||
         ( ohdr_ver_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);
    }

    ohdr_offset = (off_t)(base_addr + sb_ext_addr);

    if ( success ) {

        if ( ! load_buf_from_file(fd, file_len, ohdr_offset,
                                  (size_t)(H5O_SIZEOF_MAGIC + 1), buf,
                                  verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        }
    }

    if ( success ) {

        if ( buf[0] == 1 ) {

            ohdr_ver = 1;

        } else if ( HDmemcmp(buf,
                             H5O_HDR_MAGIC,
                             (size_t)H5O_SIZEOF_MAGIC) == 0 ) {

            ohdr_ver = (int)(buf[H5O_SIZEOF_MAGIC]);

        } else {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr,
                          "%s: sb_ext_addr doesn't seem to reference\n",
                          fcn_name);
                HDfprintf(stderr, "	an object header.\n");
                HDfprintf(stderr, "	base_addr = 0x%llx\n",
                          (unsigned long long)base_addr);
                HDfprintf(stderr, "	sb_ext_addr = 0x%llx\n",
                          (unsigned long long)sb_ext_addr);
            }
        }
    }

    if ( success ) {

        if ( verbosity > 2 ) {

            HDfprintf(stdout, "%s: ohdr_ver = %d.\n", fcn_name, (int)ohdr_ver);
        }

        *ohdr_ver_ptr = ohdr_ver;
    }

    return(success);

} /* get_sb_ext_obj_hdr_ver() */


/**************************************************************************
 *
 * Function:	get_sb_version
 *
 * Purpose:	Read the super block version from the supplied file with
 *		the given superblock offset, load that value into
 *		*sb_ver_ptr, and return TRUE.
 *
 *		If the function fails for any other reason, *sb_ver_ptr
 *		is undefined, and the function returns FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/24/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
get_sb_version(int fd,
               off_t file_len,
               off_t sb_offset,
               int * sb_ver_ptr)
{
    const char * fcn_name = "get_sb_version(): ";
    uint8_t ver_buf;
    hbool_t success = TRUE;
    unsigned int verbosity = 0;
    off_t version_offset;
    int version;

    if ( ( fd < 0 ) ||
         ( (sb_offset % 512) != 0 ) ||
         ( sb_ver_ptr == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);
    }

    if ( success ) {

        version_offset = sb_offset + H5F_SIGNATURE_LEN;

        if ( ! load_buf_from_file(fd, file_len, version_offset,
                                  (size_t)1, &ver_buf,
                                  verbosity, fcn_name, NULL) ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr, "%s: load_buf_from_file() failed.\n",
                          fcn_name);
            }
        } else {

            version = (int)ver_buf;

        }
    }

    if ( success ) {

        if ( verbosity > 2 ) {

            HDfprintf(stdout, "%s: version = %d.\n", fcn_name, (int)version);
        }

        *sb_ver_ptr = version;
    }

    return(success);

} /* get_sb_version() */


/**************************************************************************
 *
 * Function:	load_buf_from_file()
 *
 * Purpose:	Seek to the specified location in the supplied file and
 *		then read the indicated number of bytes into the supplied
 *		buffer.
 *
 *		Return TRUE if completely successful and FALSE otherwise.
 *		*obj_hdr_type_ptr.
 *
 *		If successful, return TRUE.  On failure for any reason,
 *		return FALSE.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 2/26/09
 *
 * Changes:	Added io_err_ptr parameter.  If defined (i.e. eof_ptr
 *		isn't NULL), this parameter allows the function to
 *		inform the caller that the failure was caused by an
 *		attempt to read beyond the end of file.
 *
 *		This is necessary, as when scanning the journal file,
 *		we will typically find it to be incomplete, and we need
 *		to distinguish between an eof error, and a system or
 *		file I/O error.
 *						JRM -- 5/22/09
 *
 **************************************************************************/

hbool_t
load_buf_from_file(int fd,
                   off_t file_len,
                   off_t offset,
                   size_t buf_len,
                   uint8_t *buf_ptr,
                   unsigned int verbosity,
                   const char * desc,
                   hbool_t * eof_ptr)
{
    const char * fcn_name = "load_buf_from_file()";
    hbool_t success = TRUE;
    ssize_t result;

    if ( ( fd < 0 ) ||
         ( file_len <= 0 ) ||
         ( buf_len <= 0 ) ||
         ( buf_ptr == NULL ) ||
         ( desc == NULL ) ) {

        success = FALSE;
	HDfprintf(stderr, "%s bad params on entry.\n", fcn_name);

    } else if ( file_len < offset + (off_t)buf_len ) {

        success = FALSE;

        if ( eof_ptr != NULL ) {

            *eof_ptr = TRUE;
        }

        if ( verbosity > 2 ) {

            HDfprintf(stderr, "%s:%s: file too short.\n", fcn_name, desc);
        }
    }

    if ( success ) {

        if ( offset != HDlseek(fd, offset, SEEK_SET) ) {

            success = FALSE;

            if ( verbosity > 2 ) {

                HDfprintf(stderr,
                        "%s:%s: lseek() to 0x%llx failed with errno %d(%s).\n",
                        fcn_name,
                        desc,
                        (unsigned long long)offset,
                        errno,
                        strerror(errno));
            }
        } else {

	    result = HDread(fd, (void *)buf_ptr, buf_len);

            if ( result == -1 ) {

                success = FALSE;

                if ( verbosity > 2 ) {

                    HDfprintf(stderr,
                          "%s:%s: read() at 0x%llx failed with errno %d(%s).\n",
                          fcn_name,
                          desc,
                          (unsigned long long)offset,
                          errno,
                          strerror(errno));
                }
            } else if ( result != (ssize_t)buf_len ) {

                success = FALSE;

                if ( verbosity > 2 ) {

                    HDfprintf(stderr,
                     "%s:%s: read() at 0x%llx read wrong # of bytes: %d(%d).\n",
                     fcn_name,
                     desc,
                     (unsigned long long)offset,
                     (int)result,
                     (int)buf_len);
                }
            }
        }
    }

    return(success);

} /* load_buf_from_file() */


/**************************************************************************/
/***************** HDF5 Journal File Investigation Code *******************/
/**************************************************************************/
/* The following functions are used to examing the target HDF5 journal    */
/* file to determine if it is a valid journal file, if it matches the     */
/* supplied HDF5 file, and if it contains any complete transactions.      */
/**************************************************************************/

/**************************************************************************
 *
 * Function:	get_jnl_header_info
 *
 * Purpose:	Attempt to open the indicated HDF5 journal file, read its
 *		header information, and return this data to the caller
 *		in the indicated locations after closing the file.
 *
 *		If examine is TRUE, do this while maintaining a running
 *		commentary.
 *
 *		If successful, return TRUE.  On failure for any reason,
 *		return FALSE.
 *
 *		Note that the function assumes that target_file_name_ptr
 *		points to a string that is at least MAX_PATH_LEN + 1
 *		bytes long.
 *
 * Return:	Success: TRUE
 *		Failure: FALSE
 *
 * Programmer:	JRM -- 3/17/09
 *
 * Changes:	JRM -- 3/19/09
 *		Added the verbosity parameter and associated code.  It
 *		doesn't change the execution, but it does control the
 *		amount and format of comentary generated.
 *
 *		Also added file_len parameter -- now we are guaranteed
 *		that the file exists and has positive length on entry.
 *
 *		JRM -- 5/15/09
 *		Updated function to handle the extra fields that appear
 *		in the header when human_readable is false.  This included
 *		adding the offset_width_ptr and length_width_ptr fields.
 *		Note that *offset_width_ptr and *length_width_ptr are
 *		undefined if *human_readable_ptr is TRUE.
 *
 **************************************************************************/

hbool_t
get_jnl_header_info(char * file_path_ptr,
                    off_t file_len,
                    size_t * header_length_ptr,
                    int32_t * version_ptr,
                    char * target_file_name_ptr,
                    int32_t * magic_ptr,
                    hbool_t * human_readable_ptr,
                    int * offset_width_ptr,
                    int * length_width_ptr,
                    hbool_t examine,
                    unsigned verbosity)
{
    const char * fcn_name = "get_jnl_header_info()";
    const char * indent1 = "";
    const char * indent2 = "\t";
    char buf[MAX_PATH_LEN + 256 + 1];
    char target_file_name[MAX_PATH_LEN + 1];
    hbool_t human_readable;
    hbool_t success = TRUE;
    size_t header_length;
    int version;
    int offset_width;
    int length_width;
    int32_t magic;
    FILE * file_ptr = NULL;
    FILE * err_file_ptr = stderr;

    if ( ( file_path_ptr == NULL ) ||
         ( file_len <= 0 ) ||
         ( version_ptr == NULL ) ||
         ( target_file_name_ptr == NULL ) ||
         ( magic_ptr == NULL ) ||
         ( human_readable_ptr == NULL ) ||
         ( offset_width_ptr == NULL ) ||
         ( length_width_ptr == NULL ) ) {

        success = FALSE;
        HDfprintf(stderr, "%s: bad param(s) on entry.\n", fcn_name);
    }

    if ( examine ) {

        indent1 = "\t";
        indent2 = "\t\t";

        /* don't generate output to stderr during examing unless a major
         * error or a bug in the program is detected.
         */

        err_file_ptr = stdout;

    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to open %s...\n",
                      indent1, file_path_ptr);
        }

        /* In the following call, the mode should be ignored. */
        file_ptr = HDfopen(file_path_ptr, "r");

        if ( file_ptr == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sCan't open %s -- permissions?.\n",
                      indent1, file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sopened file successfully.\n", indent2);
            }
        }
    }

    if ( success ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to read header.\n", indent1);
        }

        if ( HDfgets(buf, MAX_PATH_LEN + 256, file_ptr) == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sCan't read first line of %s.\n",
                      indent1, file_path_ptr);

        } else if ( buf[0] != '0' ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
		      "%sbad first char of header -- not a journal file.\n",
                      indent1);
        } else {

            header_length = strlen(buf);

            if ( buf[(int)header_length - 1] != '\n' ) {

                success = FALSE;
                HDfprintf(err_file_ptr,
                          "%sheader line too long -- not a journal file.\n",
                          indent1);
            }
        }
    }

    if ( success ) { /* get version */

        char * version_tag = NULL;
        char * version_str = NULL;

        version_tag = HDstrtok(&(buf[1]), " ");

        if ( ( version_tag == NULL ) ||
             ( strcmp(version_tag, H5C2_JNL__VER_NUM_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sunexpected version tag \"%s\".\n",
                      indent1, version_tag );

        } else if ( (version_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sno version string in header.\n",
                      indent1);

        } else if ( (version = atoi(version_str)) != 1 ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%sunknown journal file version \"%s\"(%d).\n",
                      indent1, version_str, version);

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sjournal file version = %d.\n",
                      indent2, version);
        }
    }

    if ( success ) { /* get target file name */

        char * target_file_tag = NULL;
        char * target_file_str = NULL;

        target_file_tag = HDstrtok(NULL, " ");

        if ( ( target_file_tag == NULL ) ||
             ( strcmp(target_file_tag, H5C2_JNL__TGT_FILE_NAME_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sUnexpected target file tag \"%s\".\n",
                      indent1, target_file_tag );

        } else if ( (target_file_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sno target file string in header.\n",
                      indent1);

        } else if ( strlen(target_file_str) > MAX_PATH_LEN ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                          "%starget file path exceeds MAX_PATH_LEN.\n",
                          indent1);

        } else {

            HDstrcpy(target_file_name, target_file_str);

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%starget file = \"%s\"\n",
                          indent2, target_file_name);
            }
        }
    }

    if ( success ) { /* get journal magic */

        char * journal_magic_tag = NULL;
        char * journal_magic_str = NULL;
        long tmp_magic;

        journal_magic_tag = HDstrtok(NULL, " ");

        if ( ( journal_magic_tag == NULL ) ||
             ( strcmp(journal_magic_tag, H5C2_JNL__JNL_MAGIC_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sUnexpected journal magic tag \"%s\".\n",
                      indent1, journal_magic_tag );

        } else if ( (journal_magic_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;


            HDfprintf(err_file_ptr, "%sno journal magic string in header.\n",
                      indent1);

        } else if ( ( (tmp_magic = HDstrtol(journal_magic_str, NULL, 10))
                      == LONG_MIN ) ||
                    ( tmp_magic == LONG_MAX ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%serror reading magic %s (%d)\n",
                      indent1, errno, strerror(errno));

        } else {

            magic = (int32_t)tmp_magic;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%smagic = 0x%x\n", indent2, magic);
            }
        }
    }

    if ( success ) { /* get creation date */

        char * creation_date_tag = NULL;
        char * creation_date_str = NULL;

        creation_date_tag = HDstrtok(NULL, " ");

        if ( ( creation_date_tag == NULL ) ||
             ( strcmp(creation_date_tag, H5C2_JNL__CREATION_DATE_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%sUnexpected journal creation date tag \"%s\".\n",
                      indent1, creation_date_tag );

        } else if ( (creation_date_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sno creation date string in header.\n",
                      indent1);

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "%screation date = %s\n",
                      indent2, creation_date_str);
        }
    }

    if ( success ) { /* get human readable */

        char * human_readable_tag = NULL;
        char * human_readable_str = NULL;
        long tmp_human_readable;

        human_readable_tag = HDstrtok(NULL, " ");

        if ( ( human_readable_tag == NULL ) ||
             ( strcmp(human_readable_tag, H5C2_JNL__HUMAN_READABLE_TAG)
               != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%sUnexpected human readable tag \"%s\".\n",
                      indent1, human_readable_tag );

        } else if ( (human_readable_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sno human readable string in header.\n",
                      indent1);

        } else if ( ( (tmp_human_readable =
                       HDstrtol(human_readable_str, NULL, 10)) == LONG_MIN ) ||
                    ( tmp_human_readable == LONG_MAX ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%serror decoding human readable string: %d (%s)\n",
                      indent1, errno, strerror(errno));

        } else if ( ( tmp_human_readable != 0 ) &&
                    ( tmp_human_readable != 1 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr,
                      "%shuman readable value out of range (%ld).\n",
                      indent1, tmp_human_readable);

        } else {

            human_readable = (hbool_t)tmp_human_readable;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%shuman readable = %d\n",
                          indent2, human_readable);
            }
        }
    }

    if ( ( success ) && ( ! human_readable ) )  { /* get offset width */

        char * offset_width_tag = NULL;
        char * offset_width_str = NULL;
        long tmp_offset_width;

        offset_width_tag = HDstrtok(NULL, " ");

        if ( ( offset_width_tag == NULL ) ||
             ( strcmp(offset_width_tag, H5C2_JNL__OFFSET_WIDTH_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sUnexpected offset width tag \"%s\".\n",
                      indent1, offset_width_tag );

        } else if ( (offset_width_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;


            HDfprintf(err_file_ptr, "%sno offset width string in header.\n",
                      indent1);

        } else if ( ( (tmp_offset_width = HDstrtol(offset_width_str, NULL, 10))
                      == LONG_MIN ) ||
                    ( tmp_offset_width == LONG_MAX ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%serror reading offset width %s (%d)\n",
                      indent1, errno, strerror(errno));

        } else if ( ! ( ( tmp_offset_width == 2 ) ||
                        ( tmp_offset_width == 4 ) ||
                        ( tmp_offset_width == 8 ) ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sInvalid offset width %d.\n",
                      indent1, (int)tmp_offset_width);

        } else {

            offset_width = (int)tmp_offset_width;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%soffset width = %d\n", indent2, offset_width);
            }
        }
    }

    if ( ( success ) && ( ! human_readable ) )  { /* get length width */

        char * length_width_tag = NULL;
        char * length_width_str = NULL;
        long tmp_length_width;

        length_width_tag = HDstrtok(NULL, " ");

        if ( ( length_width_tag == NULL ) ||
             ( strcmp(length_width_tag, H5C2_JNL__LENGTH_WIDTH_TAG) != 0 ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sUnexpected length width tag \"%s\".\n",
                      indent1, length_width_tag );

        } else if ( (length_width_str = HDstrtok(NULL, " ")) == NULL ) {

            success = FALSE;


            HDfprintf(err_file_ptr, "%sno length width string in header.\n",
                      indent1);

        } else if ( ( (tmp_length_width = HDstrtol(length_width_str, NULL, 10))
                      == LONG_MIN ) ||
                    ( tmp_length_width == LONG_MAX ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%serror reading length width %s (%d)\n",
                      indent1, errno, strerror(errno));

        } else if ( ! ( ( tmp_length_width == 2 ) ||
                        ( tmp_length_width == 4 ) ||
                        ( tmp_length_width == 8 ) ) ) {

            success = FALSE;

            HDfprintf(err_file_ptr, "%sInvalid length width %d.\n",
                      indent1, (int)tmp_length_width);

        } else {

            length_width = (int)tmp_length_width;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%slength width = %d\n", indent2, length_width);
            }
        }
    }

    if ( file_ptr != NULL ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sattempting to close %s...\n",
                      indent1, file_path_ptr);
        }

        if ( HDfclose(file_ptr) != 0 ) {

            HDfprintf(stderr, "%serror closing %s.\n",
                      indent1, file_path_ptr);

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "%sclosed file successfully.\n", indent2);
        }
    }

    if ( success ) { /* copy results into provided locations */

        *header_length_ptr = header_length;
        *version_ptr = version;
        strcpy(target_file_name_ptr, target_file_name);
        *magic_ptr = magic;
        *human_readable_ptr = human_readable;

        if ( ! human_readable ) {

	    *offset_width_ptr = offset_width;
            *length_width_ptr = length_width;
        }
    }

    return(success);

} /* get_jnl_header_info() */


/**************************************************************************
 *
 * Function:	scan_journal_file
 *
 * Purpose:	Scan the supplied journal file to determine whether it
 *		contains any completed transactions.  If it does, also
 *		report the maximum address that appears in an eoa message,
 *		the size of the largest journal entry body, and the
 *		number of the last complete transaction.
 *
 *		The function assumes that the header has already been
 *		examined, and that its length is in the header_length.
 *		If the journal file is binary, it also assumes that the
 *		offset_width and length_width parameters contain the
 *		widths of offsets and lengths in the binary journal file
 *		(and also in the target HDF5 file).
 *
 *		The function also assumes that the journal file is
 *		closed on entry.
 *
 *		Note: While this function has been designed with both
 *		      human readable and binary journal files in mind,
 *		      in its first incarnation, it only supports binary
 *		      journal files.
 *
 * Return:	TRUE if no errors are detected, and FALSE otherwise.
 *
 * Programmer:	JRM -- 5/20/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
scan_journal_file(char * file_path_ptr,
                  off_t file_len,
                  size_t header_length,
                  hbool_t human_readable,
                  int hdf5_offset_width,
                  int hdf5_length_width,
                  uint64_t * max_trans_num_ptr,
                  haddr_t * max_eoa_ptr,
                  size_t * max_body_len_ptr,
                  unsigned verbosity)
{
    const char * fcn_name = "scan_journal_file()";
    hbool_t trans_in_progress = FALSE;
    hbool_t proceed = TRUE;
    int fd;
    int msg_type = JF__UNDEFINED_MSG;
    size_t hdf5_length;
    size_t max_hdf5_length = 0;
    haddr_t eoa;
    haddr_t max_eoa = HADDR_UNDEF;
    haddr_t hdf5_offset;
    uint64_t cur_trans_num;
    uint64_t trans_num = 0;
    uint64_t max_trans_num = 0;
    off_t offset;
    off_t new_offset;


    if ( ( file_path_ptr == NULL ) ||
         ( file_len <= 0 ) ||
         ( max_trans_num_ptr == NULL ) ||
         ( max_eoa_ptr == NULL ) ||
         ( max_body_len_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stdout, "%s: bad params on entry.\n", fcn_name);

    } else if ( ( body_buf != NULL ) || ( body_buf_len != 0 ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: body buffer already allocated?!?\n", fcn_name);

    } else {

        body_buf_len = 1024;
        body_buf = (char *)HDmalloc(body_buf_len);

        if ( body_buf == NULL ) {

            body_buf_len = 0;
            proceed = FALSE;
            HDfprintf(stderr, "%s: initial allocation of body buffer failed.\n",
                      fcn_name);
        }
    }

    if ( ( proceed ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout, "Scanning journal file entries:\n");
    }

    if ( proceed ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to open %s...\n", file_path_ptr);
        }


        /* In the following call, the mode should be ignored. */
        fd = HDopen(file_path_ptr, O_RDONLY, S_IRUSR);

        if ( fd == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "\tcan't open %s.\n", file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "\topened file successfully.\n");
            }
        }
    }

    if ( proceed ) {

        offset = (off_t)header_length;
    }

    while ( ( proceed ) && ( msg_type != JF__INVALID_MSG ) ) {

        if ( human_readable ) {

            HDfprintf(stderr, "%s: human readable not yet implemented.\n",
                      fcn_name);
            proceed = FALSE;

        } else {

            proceed = bjf__load_next_msg(fd,
                                         file_len,
                                         offset,
                                         &new_offset,
                                         &msg_type,
                                         &trans_num,
                                         &hdf5_offset,
                                         hdf5_offset_width,
                                         &hdf5_length,
                                         hdf5_length_width,
                                         &eoa,
                                         NULL,
                                         (size_t)0,
                                         verbosity);
        }

        if ( proceed ) {

            switch ( msg_type ) {

                case JF__UNDEFINED_MSG:
                    proceed = FALSE;
                    HDfprintf(stderr,
                              "%s: bjf__load_next_msg() returned undef msg?!?\n",
                              fcn_name);
		    break;

		case JF__BEGIN_TRANS_MSG:
                    if ( trans_in_progress ) {

                        /* At present, nested transaction are forbidden --
                         * scream and die if we encounter one.
                         */

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                  "Encountered nested transaction in journal");

                    } else if ( trans_num != max_trans_num + 1 ) {

                        /* we may decide to drop this invarient eventually,
                         * but we will check it for now, and scream and die
                         * if it fails.
                         */
                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                           "Encountered out of sequence transaction in journal");

                    } else {

                        trans_in_progress = TRUE;
                        cur_trans_num = trans_num;

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "\tBegin Trans: %lld.\n",
                                      (long long)cur_trans_num);
                        }
                    }
		    break;

		case JF__JNL_ENTRY_MSG:
                    if ( ! trans_in_progress ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered jnl entry outside of transaction");

                    } else if ( trans_num != cur_trans_num ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered jnl entry with bad trans num");

                    } else {

                        if ( hdf5_length > max_hdf5_length ) {

                             max_hdf5_length = hdf5_length;
                        }

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "\tJnl Entry: trans = %lld, ",
                                      (long long)cur_trans_num);

                            HDfprintf(stdout, "offset = 0x%llx, len = 0x%llx.\n",
                                      (unsigned long long)hdf5_offset,
                                      (unsigned long long)hdf5_length);
                        }
                    }
		    break;

		case JF__END_TRANS_MSG:
                    if ( ! trans_in_progress ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                             "Encountered trans end msg outside of transaction");

                    } else if ( trans_num != cur_trans_num ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered trans end with bad trans num");

                    } else {

                       trans_in_progress = FALSE;
                       max_trans_num = cur_trans_num;

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "\tBegin Trans: %lld.\n",
                                      (long long)cur_trans_num);
                        }
                    }
		    break;

		case JF__EOA_MSG:
                    if ( ( max_eoa == HADDR_UNDEF ) ||
                         ( max_eoa < eoa ) ) {

                        max_eoa = eoa;
                    }

                    if ( verbosity > 1 ) {

                        HDfprintf(stdout, "\tEOA message: 0x%llx.\n",
                                  (unsigned long long)eoa);
                    }
		    break;

		case JF__INVALID_MSG:
                    if ( verbosity > 1 ) {

                        HDfprintf(stdout,
                                  "\tinvalid message -- end of journal.\n");
                    }
		    break;

		default:
                    proceed = FALSE;
                    HDfprintf(stderr,
                           "%s: bjf__load_next_msg() returned unknown msg?!?\n",
                           fcn_name);
		    break;

            }
        }

        if ( proceed ) {

            offset = new_offset;
        }
    }

    if ( fd != -1 ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to close %s...\n", file_path_ptr);
        }

        if ( HDclose(fd) != 0 ) {

            HDfprintf(stderr, "%s: Error closing %s.  errno = %s (%s).\n",
                      fcn_name, file_path_ptr, errno, HDstrerror(errno));

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tclosed file successfully\n");
        }
    }


    if ( proceed ) {

        *max_trans_num_ptr = max_trans_num;
        *max_eoa_ptr       = max_eoa;
        *max_body_len_ptr  = max_hdf5_length;

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "Finished scanning journal file:\n");

            if ( max_trans_num == 0 ) {

                HDfprintf(stdout, "\tNo complete transactions.\n");

            } else {

                HDfprintf(stdout, "\t%lld complete transactions.\n",
                          (unsigned long long)max_trans_num);
                HDfprintf(stdout, "\tmax journal entry body length = %lld.\n",
                          (long long)max_hdf5_length);
            }

            HDfprintf(stdout, "\tMax eoa = 0x%llx.\n",
                      (unsigned long long)max_eoa);
        }
    }

    if ( body_buf != NULL ) {

        HDfree(body_buf);
        body_buf = NULL;
        body_buf_len = 0;
    }

    return(proceed);

} /* scan_journal_file() */


/**************************************************************************/
/*********************** File Examination Code ****************************/
/**************************************************************************/
/* The following functions are used to support the examine function -- in */
/* which the supplied files are examined to determine their status, but   */
/* no action is taken.                                                    */
/**************************************************************************/

/**************************************************************************
 *
 * Function:	examine_files()
 *
 * Purpose:	Scan the supplied file(s) to determine their status,
 *		and report.  Do nothing.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 3/17/09
 *
 * Changes:	None.
 *
 **************************************************************************/

void
examine_files(char * hdf5_file_name,
              char * journal_file_name,
              unsigned verbosity)
{
    /* const char * fcn_name = "examine_files()"; */
    const char * indent1 = "\t";
    const char * indent2 = "\t\t";
    char jnl_file_name[H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    char jnl_target_file_name[MAX_PATH_LEN + 1];
    hbool_t error = FALSE;
    hbool_t journal_can_be_run = FALSE;
    hbool_t journal_file_exists = FALSE;
    hbool_t journal_file_ok = FALSE;
    hbool_t jnl_human_readable;
    int sb_version;
    int offset_size;
    int length_size;
    int jnl_offset_size;
    int jnl_length_size;
    int jnl_file_version;
    int32_t magic;
    int32_t jnl_magic;
    size_t header_length = 0;
    off_t eoa_offset;
    off_t sb_offset;
    off_t hdf5_file_len;
    off_t journal_file_len = 0;
    haddr_t base_addr;

    if ( hdf5_file_name == NULL ) {

        HDfprintf(stdout, "No target HDF5 file supplied.\n");

    } else {

        journal_can_be_run = get_journaling_status(hdf5_file_name,
                                                   &hdf5_file_len,
                                                   &sb_offset,
                                                   &sb_version,
                                                   &base_addr,
                                                   &offset_size,
                                                   &length_size,
                                                   &eoa_offset,
                                                   &magic,
                                                   jnl_file_name,
                                                   &error,
                                                   TRUE,
                                                   verbosity);
        if ( error ) {

            HDfprintf(stdout,
                "\nEncountered errors examining %s -- can't run journal.\n",
                hdf5_file_name);
        }
    }

    if ( journal_file_name == NULL ) {

        HDfprintf(stdout, "\nNo target journal file supplied -- exiting.\n");

    } else {

        HDfprintf(stdout, "\nExamining the putative HDF5 journal file %s:\n",
                  journal_file_name);

        if ( verbosity > 0 ) {

            HDfprintf(stdout,
                      "%sattempting to determine journal file length...\n",
                      indent1);
        }

        if ( ! get_file_len(journal_file_name, &journal_file_len) ) {

            journal_file_exists = FALSE;
            journal_file_ok = FALSE;
            HDfprintf(stdout,
                      "%sCan't get journal file length -- does it exist?\n\n",
                      indent1);

        } else if ( journal_file_len == 0 ) {

            journal_file_exists = TRUE;

            HDfprintf(stdout, "%sthe file %s is empty.\n\n",
                      indent1, journal_file_name);

        } else {

            journal_file_exists = TRUE;

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "%sfile length = %lld\n",
                          indent2, (long long)journal_file_len);
            }
        }
    }

    if ( ( journal_file_exists) && ( journal_file_len > 0 ) ) {


        journal_file_ok = get_jnl_header_info(journal_file_name,
                                              journal_file_len,
                                              &header_length,
                                              &jnl_file_version,
                                              jnl_target_file_name,
                                              &jnl_magic,
                                              &jnl_human_readable,
                                              &jnl_offset_size,
                                              &jnl_length_size,
                                              TRUE,
                                              verbosity);

        if ( ! journal_file_ok ) {

            HDfprintf(stdout, "\nerrors reading %s.  Journal un-useable.\n\n",
                      journal_file_name);

        } else {

            HDfprintf(stdout, "%sheader in %s looks OK.\n\n",
                      indent1, journal_file_name);

        }
    }

    if ( journal_can_be_run ) {

        if ( journal_file_ok ) {

            if ( magic == jnl_magic ) {

                HDfprintf(stdout, "magics match -- %s seems to be the\n",
                          journal_file_name);
                HDfprintf(stdout, "journal file generated by %s.\n",
                          hdf5_file_name);
                HDfprintf(stdout, "Recomend running %s on %s.\n\n",
                          journal_file_name, hdf5_file_name);

            } else {

                HDfprintf(stdout, "magic mismatch!!! Don't run %s on %s.\n\n",
                          journal_file_name, hdf5_file_name);
            }
        } else if ( ( journal_file_exists ) &&
                    ( journal_file_len == 0 ) ) {

            HDfprintf(stdout,
"Since %s is empty, either %s\n\
already has consistent metadata and need only be marked as not having\n\
journaling in progress so it can opened with the hdf5 library, or\n\
%s is not really its journal file.\n\n\
Unfortunately, which is true can't be determined by this program.\n\
If you are sure you have the correct journal file, go ahead and run\n\
%s on %s.\n\n",
                     journal_file_name,
                     hdf5_file_name,
                     journal_file_name,
                     journal_file_name,
                     hdf5_file_name);
        }
    }

    return;

} /* examine_files() */


/**************************************************************************/
/*********************** File Verification Code ***************************/
/**************************************************************************/
/* The following function(s) are used to verify that the supplied journal */
/* file matches the supplied HDF5 file, and cancel the recovery if this   */
/* is not the case.                                                       */
/**************************************************************************/

/**************************************************************************
 *
 * Function:	verify_files()
 *
 * Purpose:	Examine the supplied HDF5 file, and determine if it is:
 *
 *			a) really an HDF5 file,
 *
 *			b) uses a version of the HDF5 file format that
 *			   supports journaling, and
 *
 *			c) is marked as having journaling in progress.
 *
 *		If any of these conditions fails to hold, there is nothing
 *		to be done -- issue a diagnostic and return false.
 *
 *		If all the above conditions hold, examine the supplied
 *		journal file and determine if it is either:
 *
 *			a) and empty file, or
 *
 *			b) an HDF5 journal file with magic number matching
 *			   that of the supplied HDF5 file.
 *
 *		In the first case, if the force flag is not set, notify the
 *		user that the journal file is empty, that we therefore
 *		cannot verify that it is the correct journal file for the
 *		supplied HDF5 file, and give the user the chance to abort
 *		the recovery.  Return false if the the recovery is aborted,
 *		and true if it is allowed to proceed.
 *
 *		In the second case, just return true
 *
 *		If neither condition holds, issue an diagnostic and return
 *		false.
 *
 * Return:	true if the recovery is to proceed, and false otherwise.
 *
 * Programmer:	JRM -- 3/23/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
verify_files(char * hdf5_file_name,
             off_t * hdf5_file_len_ptr,
             off_t * hdf5_sb_offset_ptr,
             int * hdf5_sb_ver_ptr,
             haddr_t * hdf5_base_addr_ptr,
             char * journal_file_name,
             off_t * jnl_file_len_ptr,
             size_t * jnl_header_len_ptr,
             hbool_t force,
             hbool_t * human_readable_ptr,
             int * offset_width_ptr,
             int * length_width_ptr,
             unsigned verbosity,
             hbool_t * error_ptr)
{
    const char * fcn_name = "verify_files()";
    char jnl_file_name[H5C2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    char jnl_target_file_name[MAX_PATH_LEN + 1];
    hbool_t proceed = TRUE;
    hbool_t journal_file_exists = FALSE;
    hbool_t jnl_human_readable = TRUE;
    int hdf5_offset_size;
    int hdf5_length_size;
    int jnl_offset_size = 0; /* a convenient, invalid value */
    int jnl_length_size = 0; /* a convenient, invalid value */
    int jnl_file_version;
    int32_t magic;
    int32_t jnl_magic;
    size_t header_length = 0;
    off_t eoa_offset;
    off_t journal_file_len = 0;

    if ( ( hdf5_file_name == NULL ) ||
         ( hdf5_file_len_ptr == NULL ) ||
         ( hdf5_sb_offset_ptr == NULL ) ||
         ( hdf5_sb_ver_ptr == NULL ) ||
         ( hdf5_base_addr_ptr == NULL ) ||
         ( journal_file_name == NULL ) ||
         ( jnl_file_len_ptr == NULL ) ||
         ( jnl_header_len_ptr == NULL ) ||
         ( human_readable_ptr == NULL ) ||
         ( offset_width_ptr == NULL ) ||
         ( length_width_ptr == NULL ) ||
         ( error_ptr == NULL ) ||
         ( *error_ptr != FALSE ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: bad param(s) on entry.\n", fcn_name);

    }

    if ( proceed ) {

        proceed = get_journaling_status(hdf5_file_name,
                                        hdf5_file_len_ptr,
                                        hdf5_sb_offset_ptr,
                                        hdf5_sb_ver_ptr,
                                        hdf5_base_addr_ptr,
                                        &hdf5_offset_size,
                                        &hdf5_length_size,
                                        &eoa_offset,
                                        &magic,
                                        jnl_file_name,
                                        error_ptr,
                                        FALSE,
                                        verbosity);

        if ( *error_ptr ) {

            HDfprintf(stderr,
                  "\nEncountered errors examining %s -- can't run journal.\n\n",
                  hdf5_file_name);

        } else if ( ! proceed ) {

            HDfprintf(stderr,
                      "\nJournaling not enabled and/or supported on %s\n",
                      hdf5_file_name);
            HDfprintf(stderr, "Can't run journal.\n\n");

        } else {

            HDfprintf(stdout,
                      "\n%s was journaled and not closed properly.\n",
                      hdf5_file_name);

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "\tmagic = 0x%lx\n", (unsigned long)magic);
                HDfprintf(stdout, "\tjournal file name = \"%s\".\n",
                          jnl_file_name);
            }
        }
    }

    if ( proceed ) {

        if ( ! get_file_len(journal_file_name, &journal_file_len) ) {

            proceed = FALSE;
            *error_ptr = TRUE;
            journal_file_exists = FALSE;
            HDfprintf(stdout,
                      "\nCan't get journal file length -- does it exist?\n\n");

        } else {

            journal_file_exists = TRUE;

            *jnl_file_len_ptr = journal_file_len;
        }
    }

    if ( ( proceed ) &&
         ( journal_file_exists) ) {

        if ( journal_file_len > 0 ) {

            proceed = get_jnl_header_info(journal_file_name,
                                          journal_file_len,
                                          &header_length,
                                          &jnl_file_version,
                                          jnl_target_file_name,
                                          &jnl_magic,
                                          &jnl_human_readable,
                                          &jnl_offset_size,
                                          &jnl_length_size,
                                          FALSE,
                                          verbosity);

            if ( ! proceed ) {

                *error_ptr = TRUE;
                HDfprintf(stderr,
                          "\nerrors reading %s.  Journal un-useable.\n\n",
                          journal_file_name);

            } else {

                *jnl_header_len_ptr = header_length;
                *human_readable_ptr = jnl_human_readable;
                *offset_width_ptr = jnl_offset_size;
                *length_width_ptr = jnl_length_size;

                if ( magic == jnl_magic ) {

                    HDfprintf(stdout,
                               "\nthe journal file %s has matching magic.\n\n",
                               journal_file_name);

                    if ( ( ! jnl_human_readable ) &&
                         ( ( jnl_offset_size != hdf5_offset_size ) ||
                           ( jnl_length_size != hdf5_length_size ) ) ) {

                        proceed = FALSE;
                        *error_ptr = TRUE;
                        HDfprintf(stderr, "\n%s %s -- %s\n\n",
                                 "journal and hdf5 file offset and/or",
                                 "length size mismatch",
                                 "recovery canceled.");
                        HDfprintf(stderr, "\thdf5/jnl offset size = %d/%d\n",
			          hdf5_offset_size, jnl_offset_size);
                        HDfprintf(stderr, "\thdf5/jnl length size = %d/%d\n",
			          hdf5_length_size, jnl_length_size);

                    }

                } else {

                    proceed = FALSE;
                    *error_ptr = TRUE;
                    HDfprintf(stderr, "\n%s %s\n\n",
                              "journal and file magic don't match --",
                              "recovery canceled.");
                }
            }
        } else {

            HDfprintf(stdout, "The journal file %s is empty.\n",
                      journal_file_name);
            HDfprintf(stdout, "%s %s\n",
                      "Thus is is impossible to verify that",
                      "it is the matching journal file.");

            if ( force ) {

                HDfprintf(stdout, "Recovery forced.\n");

            } else {

                char reply_buf[128];
                hbool_t done = FALSE;
                int i;

                while ( ! done ) {

                    HDfprintf(stdout, "Proceed with the recovery? (y/n): ");

                    if ( HDfgets(reply_buf, 127, stdin) != 0 ) {

                        i = 0;
                        while ( ( i < 128 ) && ( isspace(reply_buf[i]) ) )
                        {
                            i++;
                        }

                        if ( ( reply_buf[i] == 'y' ) ||
                             ( reply_buf[i] == 'Y' ) ) {

                            done = TRUE;
                            HDfprintf(stdout, "Proceeding with recovery.\n");

                        } else if ( ( reply_buf[i] == 'n' ) ||
                                    ( reply_buf[i] == 'N' ) ) {

                            done = TRUE;
                            proceed = FALSE;
                            HDfprintf(stdout, "Recovery canceled.\n");

                        }
                    }
                }
            }

            *human_readable_ptr = TRUE;
        }
    }

    return(proceed);

} /* verify_files() */


/**************************************************************************/
/******************** HDF5 Journal Application Code ***********************/
/**************************************************************************/
/* The following functions are used in the actual application of a        */
/* journal file to a corrupted hdf5 file.                                 */
/**************************************************************************/

/**************************************************************************
 *
 * Function:	apply_journal_file()
 *
 * Purpose:	Apply the supplied journal file to the supplied (presumably
 *		corrupt) HDF5 fie.
 *
 *		The function assumes that:
 *
 *		1) The supplied journal file and HDF5 file have been
 *		   examined, and that their magic numbers and offset
 *		   and length sizes match.
 *
 *		2) That the journal file contains at least one complete
 *		   transaction.
 *
 *		3) The max eoa listed in the journal file is known, as
 *		   is the offset of the eoa in the hdf5 file.
 *
 *		The function opens the supplied HDF5 and journal files,
 *		applies in order all journal entries from all completed
 *		transactions, closes the supplied files, and then marks
 *		the HDF5 file as being recovered.
 *
 *		Note: While this function has been designed with both
 *		      human readable and binary journal files in mind,
 *		      in its first incarnation, it only supports binary
 *		      journal files.
 *
 * Return:	TRUE if no errors are detected, and FALSE otherwise.
 *
 * Programmer:	JRM -- 5/26/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
apply_journal_file(char * hdf5_file_path_ptr,
                   off_t hdf5_file_len,
                   off_t sb_offset,
                   int sb_ver,
                   haddr_t hdf5_base_addr,
                   char * jnl_file_path_ptr,
                   off_t jnl_file_len,
                   size_t header_length,
                   hbool_t human_readable,
                   int hdf5_offset_width,
                   int hdf5_length_width,
                   uint64_t max_trans_num,
                   haddr_t new_eoa,
                   size_t max_body_len,
                   unsigned verbosity)
{
    const char * fcn_name = "scan_journal_file()";
    char * buf = NULL;
    hbool_t done = FALSE;
    hbool_t trans_in_progress = FALSE;
    hbool_t proceed = TRUE;
    int hdf5_fd;
    int jnl_fd;
    int msg_type = JF__UNDEFINED_MSG;
    ssize_t bytes_written;
    size_t hdf5_length;
    haddr_t eoa;
    haddr_t hdf5_offset;
    uint64_t cur_trans_num;
    uint64_t last_trans_num = 0;
    uint64_t trans_num = 0;
    off_t offset;
    off_t new_offset;


    if ( ( hdf5_file_path_ptr == NULL ) ||
         ( jnl_file_path_ptr == NULL ) ||
         ( jnl_file_len <= 0 ) ||
         ( header_length <= 0 ) ||
         ( max_trans_num <= 0 ) ||
         ( new_eoa == HADDR_UNDEF ) ||
         ( max_body_len <= 0 ) ) {

        proceed = FALSE;
        HDfprintf(stdout, "%s: bad params on entry.\n", fcn_name);

    } else if ( human_readable ) {

        proceed = FALSE;
        HDfprintf(stderr,
                  "%s: only binary journal files supported at present.\n",
                  fcn_name);
    }

    if ( ( proceed ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout, "Applying journal file %s to HDF5 file %s:\n",
                  jnl_file_path_ptr, hdf5_file_path_ptr);
    }

    if ( proceed ) {

        if ( verbosity > 1 ) {

            HDfprintf(stdout,
                    "\tattempting to allocate journal entry body buffer...\n");
        }

        buf = (char *)HDmalloc(max_body_len + 1);

        if (buf == NULL) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: buffer allocation failed.\n", fcn_name);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "\tbuffer allocation successful.\n");
            }
        }
    }

    if ( proceed ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to open journal file %s...\n",
                      jnl_file_path_ptr);
        }

        /* In the following call, the mode should be ignored. */
        jnl_fd = HDopen(jnl_file_path_ptr, O_RDONLY, S_IRUSR);

        if ( jnl_fd == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "\tcan't open jnl file %s.\n", jnl_file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "\topened jnl file successfully.\n");
            }
        }
    }

    if ( proceed ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to open HDF5 file %s...\n",
                      hdf5_file_path_ptr);
        }


        /* In the following call, the mode should be ignored. */
        hdf5_fd = HDopen(hdf5_file_path_ptr, O_RDWR, S_IRUSR);

        if ( hdf5_fd == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "\tcan't open hdf5 file %s.\n",
                      hdf5_file_path_ptr);

        } else {

            if ( verbosity > 0 ) {

                HDfprintf(stdout, "\topened hdf5 file successfully.\n");
            }
        }
    }

    if ( proceed ) {

        offset = (off_t)header_length;
    }

    if ( ( proceed ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout,
                  "Applying %lld completed transaction to the hdf5 file %s:\n",
                  (long long)max_trans_num, hdf5_file_path_ptr);
    }

    while ( ( proceed ) && ( ! done ) ) {

        if ( human_readable ) {

            HDfprintf(stderr, "%s: human readable not yet implemented.\n",
                      fcn_name);
            proceed = FALSE;

        } else {

            proceed = bjf__load_next_msg(jnl_fd,
                                         jnl_file_len,
                                         offset,
                                         &new_offset,
                                         &msg_type,
                                         &trans_num,
                                         &hdf5_offset,
                                         hdf5_offset_width,
                                         &hdf5_length,
                                         hdf5_length_width,
                                         &eoa,
                                         buf,
                                         max_body_len,
                                         verbosity);
        }

        if ( proceed ) {

            switch ( msg_type ) {

                case JF__UNDEFINED_MSG:
                    proceed = FALSE;
                    HDfprintf(stderr,
                             "%s: bjf__load_next_msg() returned undef msg?!?\n",
                             fcn_name);
		    break;

		case JF__BEGIN_TRANS_MSG:
                    if ( trans_in_progress ) {

                        /* At present, nested transaction are forbidden --
                         * scream and die if we encounter one.
                         */

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                  "Encountered nested transaction in journal");

                    } else if ( trans_num != last_trans_num + 1 ) {

                        /* we may decide to drop this invarient eventually,
                         * but we will check it for now, and scream and die
                         * if it fails.
                         */
                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                          "Encountered out of sequence transaction in journal");

                    } else {

                        trans_in_progress = TRUE;
                        cur_trans_num = trans_num;

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "\tBegin Trans: %lld.\n",
                                      (long long)cur_trans_num);
                        }
                    }
		    break;

		case JF__JNL_ENTRY_MSG:
                    if ( ! trans_in_progress ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered jnl entry outside of transaction");

                    } else if ( trans_num != cur_trans_num ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered jnl entry with bad trans num");

                    } else {

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout,
                                      "\tApplying Jnl Entry: trans = %lld, ",
                                      (long long)cur_trans_num);

                            HDfprintf(stdout, "offset = 0x%llx, len = 0x%llx.\n",
                                      (unsigned long long)hdf5_offset,
                                      (unsigned long long)hdf5_length);
                        }

                        /* apply the journal entry */

                        bytes_written = pwrite(hdf5_fd,
                                               (const void *)buf,
                                               hdf5_length,
                                               (off_t)(hdf5_base_addr +
                                                       hdf5_offset));

                        if ( bytes_written != (ssize_t)hdf5_length ) {

                           proceed = FALSE;

                           HDfprintf(stderr, "%s %lld -- exiting.\n",
                                  "jnl entry write failed applying transaction",
                                  (long long)cur_trans_num);
                        }
                    }
		    break;

		case JF__END_TRANS_MSG:
                    if ( ! trans_in_progress ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                            "Encountered trans end msg outside of transaction");

                    } else if ( trans_num != cur_trans_num ) {

                        proceed = FALSE;
                        HDfprintf(stderr, "%s -- Exiting.\n",
                                 "Encountered trans end with bad trans num");

                    } else {

                        trans_in_progress = FALSE;
                        last_trans_num = cur_trans_num;

                        if ( cur_trans_num == max_trans_num ) {

                            done = TRUE;
                        }

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "\tEnd Trans: %lld.\n",
                                      (long long)cur_trans_num);

                            if ( done ) {

                                HDfprintf(stdout,
                                          "\tLast complete transaction applied.\n");
                            }
                        }
                    }
		    break;

		case JF__EOA_MSG:
                    if ( verbosity > 1 ) {

                        HDfprintf(stdout, "\tEOA message: 0x%llx.\n",
                                  (unsigned long long)eoa);
                    }
		    break;

		case JF__INVALID_MSG:
                    proceed = FALSE;
                    HDfprintf(stderr, "%s: encountered invalid message.\n",
                              fcn_name);
		    break;

		default:
                    proceed = FALSE;
                    HDfprintf(stderr,
                           "%s: bjf__load_next_msg() returned unknown msg?!?\n",
                           fcn_name);
		    break;

            }
        }

        if ( proceed ) {

            offset = new_offset;
        }
    }


    if ( ( proceed ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout, "Updating EOA in the hdf5 file %s:\n", hdf5_file_path_ptr);
    }


    if ( buf != NULL ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tFreeing journal entry body buffer.\n");
        }

        HDfree(buf);

        buf = NULL;

    }

    if ( jnl_fd != -1 ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to close jnl file %s...\n",
                      jnl_file_path_ptr);
        }

        if ( HDclose(jnl_fd) != 0 ) {

            HDfprintf(stderr, "%s: Error closing %s.  errno = %s (%s).\n",
                      fcn_name, jnl_file_path_ptr, errno, HDstrerror(errno));

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tclosed journal file successfully\n");
        }
    }

    if ( proceed ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "Updating EOA in HDF5 file %s.\n",
                      hdf5_file_path_ptr);
        }

        proceed = update_hdf5_eoa(hdf5_file_path_ptr,
                                  hdf5_fd,
                                  hdf5_file_len,
                                  sb_offset,
                                  hdf5_offset_width,
                                  sb_ver,
                                  hdf5_base_addr,
                                  new_eoa,
                                  verbosity);
    }

    if ( hdf5_fd != -1 ) {

        if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tattempting to close hdf5 file %s...\n",
                      hdf5_file_path_ptr);
        }

        if ( HDclose(hdf5_fd) != 0 ) {

            HDfprintf(stderr, "%s: Error closing %s.  errno = %s (%s).\n",
                      fcn_name, hdf5_file_path_ptr, errno, HDstrerror(errno));

        } else if ( verbosity > 0 ) {

            HDfprintf(stdout, "\tclosed hdf5 file successfully\n");
        }
    }

    return(proceed);

} /* apply_journal_file() */


/**************************************************************************
 *
 * Function:	mark_hdf5_file_recovered()
 *
 * Purpose:	Mark the specified hdf5 file as being recovered.
 *
 * Return:	TRUE if no errors are detected, and FALSE otherwise.
 *
 * Programmer:	JRM -- 5/26/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
mark_hdf5_file_recovered(char * hdf5_file_path_ptr)

{
    const char * fcn_name = "mark_hdf5_file_recovered()";
    hbool_t proceed = TRUE;
    hid_t fapl = -1;
    hid_t fid = -1;
    H5AC2_jnl_config_t config; /* journaling configuration */

    if ( hdf5_file_path_ptr == NULL ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: bad param(s) on entry.\n", fcn_name);
    }

    if ( proceed ) {

        /* set up appropriate fapl */
        fapl = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: H5Pcreate() failed.\n", fcn_name);

        }
    }

    if ( proceed ) {

        config.version = H5C2__CURR_AUTO_SIZE_CTL_VER;

        /* get H5AC_cache_config_t configuration from fapl */
        if ( H5Pget_jnl_config(fapl, &config) == -1) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: couldn't get mdc config from FAPL.\n",
                      fcn_name);
        }
    }

    if ( proceed ) {

        /* set journal recovered field to TRUE in mdc_config */
        config.journal_recovered = TRUE;

        /* set H5AC_cache_config_t configuration with file recovered */
        if ( H5Pset_jnl_config(fapl, &config) == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: couldn't set mdc config in FAPL.\n",
                      fcn_name);
        }
    }

    if ( proceed ) {

        /* open HDF5 file with provided fapl */
        fid = H5Fopen(hdf5_file_path_ptr, H5F_ACC_RDWR, fapl);

        if ( fid == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: couldn't open recovered HDF5 file.\n",
                      fcn_name);
        }
    }

    if ( proceed ) {

        /* close HDF5 file */
        if ( H5Fclose(fid) == -1 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: couldn't close recovered HDF5 file.\n",
                      fcn_name);
        }
    }

    return(proceed);

} /* mark_hdf5_file_recovered() */


/**************************************************************************
 *
 * Function:	update_hdf5_eoa()
 *
 * Purpose:	Modify the eoa field superblock of the supplied hdf5 file
 *		to the specified value, and update the superblock checksum
 *		accordingly.
 *
 * Return:	TRUE if no errors are detected, and FALSE otherwise.
 *
 * Programmer:	JRM -- 5/26/09
 *
 * Changes:	None.
 *
 **************************************************************************/

hbool_t
update_hdf5_eoa(char * hdf5_file_path_ptr,
                int hdf5_fd,
                off_t hdf5_file_len,
                off_t sb_offset,
                int hdf5_offset_width,
                int sb_ver,
                haddr_t base_addr,
                haddr_t new_eoa,
                unsigned verbosity)
{
    const char * fcn_name = "update_hdf5_eoa()";
    char buf[256];
    hbool_t proceed = TRUE;
    uint8_t * p;
    int version_disp;
    int eoa_disp;
    int chksum_disp;
    uint32_t chksum;
    uint32_t new_chksum;
    uint32_t old_chksum;
    size_t sb_size;
    haddr_t old_eoa;

    if ( ( hdf5_file_path_ptr == NULL ) ||
         ( hdf5_fd <= -1 ) ||
         ( sb_ver != 2 ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: bad params on entry.\n", fcn_name);
    }

    if ( ( proceed ) && ( verbosity > 0 ) ) {

        HDfprintf(stdout, "Updating hdf5 file %s super block for new EOA.\n",
                  hdf5_file_path_ptr);
    }

    if ( proceed ) {

        sb_size = 8			/* signature */
                + 1			/* version */
	        + 1			/* size of offsets */
                + 1			/* size of lengths */
                + 1 			/* flags */
                + hdf5_offset_width 	/* base address */
                + hdf5_offset_width 	/* sb ext address */
                + hdf5_offset_width 	/* EOF address */
                + hdf5_offset_width 	/* root group obj hdr address */
                + 4;			/* checksum */

        version_disp = 8; 		/* signature */

        eoa_disp = version_disp
                 + 1			/* version */
	         + 1			/* size of offsets */
                 + 1			/* size of lengths */
                 + 1 			/* flags */
                 + hdf5_offset_width  	/* base address */
                 + hdf5_offset_width; 	/* sb ext address */

        chksum_disp = eoa_disp
                    + hdf5_offset_width	/* EOF address */
                    + hdf5_offset_width;/* root group obj hdr address */

        if ( sb_size >= 256 ) {

            proceed = FALSE;
            HDfprintf(stdout, "%s: buf too small for super block.\n", fcn_name);
        }
    }

    if ( proceed ) {

        if ( verbosity > 1 ) {

            HDfprintf(stdout, "\tLoading super block.\n");
        }

        /* read the super block in from file */

	proceed = load_buf_from_file(hdf5_fd,
                                     hdf5_file_len,
                                     sb_offset,
                                     sb_size,
                                     (uint8_t *)buf,
                                     verbosity,
                                     fcn_name,
                                     NULL);
    }

    if ( proceed ) {

        /* verify signature and version */
        if ( HDmemcmp(buf,
                      H5F_SIGNATURE,
                      (size_t)H5F_SIGNATURE_LEN) != 0 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: bad super block offset?!?!\n", fcn_name);

        } else if ( (int)(buf[version_disp]) != sb_ver ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: Unexpected super block verion?!?!\n",
                      fcn_name);
        }
    }

    if ( proceed ) {

        /* read the old eoa and checksum */

        p = (uint8_t *)(&(buf[eoa_disp]));

        address_decode((size_t)hdf5_offset_width,
                       (const uint8_t **)(&p),
                       &old_eoa);

        p = (uint8_t *)(&(buf[chksum_disp]));

        old_chksum = 0;

        UINT32DECODE(p, old_chksum);

        /* verify chksum */
        chksum = H5_checksum_metadata(buf, (size_t)chksum_disp, 0);

        if ( old_chksum != chksum ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: bad SB chksum prior to update.\n", fcn_name);
        }
    }

    if ( ( proceed ) && ( verbosity > 1 ) ) {

        HDfprintf(stdout, "\tpre-update eoa = 0x%llx\n",
                  (unsigned long long)old_eoa);

        HDfprintf(stdout, "\tpre-update chksum = 0x%x\n",
                  (unsigned)old_chksum);
    }

    if ( proceed ) {

        /* write the new eoa and chksum to the in core image of the
         * super block
         */

        p = (uint8_t *)(&(buf[eoa_disp]));

        address_encode((size_t)hdf5_offset_width, &p, new_eoa);

        new_chksum = H5_checksum_metadata(buf, (size_t)chksum_disp, 0);

        p = (uint8_t *)(&(buf[chksum_disp]));

        UINT32ENCODE(p, new_chksum);
    }

    if ( ( proceed ) && ( verbosity > 1 ) ) {

        HDfprintf(stdout, "\tpost-update eoa = 0x%llx\n",
                  (unsigned long long)new_eoa);

        HDfprintf(stdout, "\tpost-update chksum = 0x%x\n",
                  (unsigned)new_chksum);
    }

    if ( proceed ) {

        if ( verbosity > 1 ) {

            HDfprintf(stdout, "\tUpdating hdf5 file length.\n");
        }

        /* use ftruncate() to set the HDF5 file length to match the
         * the new EOA.
         */

        if ( HDftruncate(hdf5_fd, (off_t)(base_addr + new_eoa)) != 0 ) {

            proceed = FALSE;
            HDfprintf(stderr, "%s: HDftruncate() failed. errno = %d (%s).\n",
                      fcn_name, errno, HDstrerror(errno));
        }
    }

    if ( proceed ) {

        if ( verbosity > 1 ) {

            HDfprintf(stdout, "\tWriting modified super block to file.\n");
        }

        if ( pwrite(hdf5_fd, (void *)buf, (size_t)sb_size, sb_offset) !=
             (ssize_t)sb_size ) {

            proceed = FALSE;
            HDfprintf(stderr,
                     "Write of modified super block to hdf5 file %s failed.\n",
                     hdf5_file_path_ptr);
        }
    }

    return(proceed);

} /* update_hdf5_eoa() */


/**************************************************************************/
/********************** Binary Journal File Code **************************/
/**************************************************************************/
/* The following function(s) are used read binary journal file messages.  */
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    bjf__chksum_entry_body()
 *
 * Purpose:     Read the specifice number of bytes from the supplied file
 *		starting at the indicated offset, checksum them, and update
 *		*chksum_ptr to reflect the checksum computed.
 *
 *		Set *new_offset_ptr to reference the first byte after the
 *		entry body.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__chksum_entry_body(int fd,
                       off_t file_len,
                       off_t offset,
                       off_t * new_offset_ptr,
  	               int * msg_type_ptr,
                       size_t body_length,
                       uint32_t * chksum_ptr,
                       unsigned verbosity)
{
    const char * fcn_name = "bjf__chksum_entry_body()";
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;
    size_t rmdr;
    off_t new_offset;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( chksum_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);

    } else if ( ( body_buf == NULL ) ||
                ( body_buf_len <= 0 ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: body_buf not set up on entry.\n", fcn_name);

    } else if ( body_length > body_buf_len ) {

        /* double the size of the body buffer until it is big enough */
        while ( body_length > body_buf_len ) {

            body_buf_len *= 2;
        }
        body_buf = (char *)HDrealloc((void *)body_buf, body_buf_len);

        if ( body_buf == NULL ) {

            proceed = FALSE;
            HDfprintf(stderr,
                      "%s: realloc() of body_buf failed.  Desired new size = %d.\n",
                      fcn_name, (int)body_buf_len);
        }
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     body_length,
                                     (uint8_t *)body_buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( ! proceed ) {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    if ( ( proceed ) && ( ! eof ) ) {

        *new_offset_ptr = offset + (off_t)body_length;

        if ( chksum_ptr != NULL ) {

            *chksum_ptr = H5_checksum_metadata((const void *)body_buf,
                                               body_length,
                                               *chksum_ptr);

        }
    }

    return(proceed);

} /* bjf__chksum_entry_body() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_begin_trans_msg()
 *
 * Purpose:     Read and validate the contents of the begin transaction
 *		message from the supplied file, and set:
 *
 *			*msg_type_ptr to JF__BEGIN_TRANS_MSG, and
 *
 *			*trans_num_ptr to the transaction number listed
 *				in the message.
 *
 *			*new_offset_ptr to the offset of the byte just
 *				after the begin transaction message.
 *
 *		The supplied offset is presumed to reference the first
 *		byte of the message after the signature and version.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_begin_trans_msg(int fd,
                          off_t file_len,
                          off_t offset,
                          off_t * new_offset_ptr,
  		          int * msg_type_ptr,
                          int msg_ver,
                          uint64_t * trans_num_ptr,
                          unsigned verbosity)
{
    const char * fcn_name = "bjf__load_begin_trans_msg()";
    hbool_t proceed = TRUE;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( trans_num_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        switch ( msg_ver ) {

	    case 0:
                *new_offset_ptr = offset;

		proceed = bjf__load_trans_num(fd,
                                              file_len,
                                              *new_offset_ptr,
                                              new_offset_ptr,
  		                              msg_type_ptr,
                                              trans_num_ptr,
                                              NULL,
                                              verbosity);

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    *msg_type_ptr = JF__BEGIN_TRANS_MSG;

                } else {

                    *msg_type_ptr = JF__INVALID_MSG;

                }
		break;

	    default:
                *msg_type_ptr = JF__INVALID_MSG;
                *new_offset_ptr = offset;

                if ( verbosity > 1 ) {

                    HDfprintf(stdout,
                       "Unknown begin trans msg ver detected at offset 0x%llx.\n",
                       (unsigned long long)offset);
                }
                break;
        }
    }

    return(proceed);

} /* bjf__load_begin_trans_msg() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_chksum()
 *
 * Purpose:     Load a checksum from the supplied journal file at the
 *		specified offset.
 *
 *		If successful, set *chksum_ptr to the newly loaded check
 *		sum and set *new_offset_ptr to reference the first byte
 *		in the journal file after the checks sum.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE if any error is
 *		detected,
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_chksum(int fd,
                 off_t file_len,
                 off_t offset,
                 off_t * new_offset_ptr,
                 int * msg_type_ptr,
                 uint32_t * chksum_ptr,
                 unsigned verbosity)
{
    const char * fcn_name = "bjf__load_chksum()";
    char buf[sizeof(uint32_t) + 1];
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;
    uint8_t * p;
    uint32_t chksum;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( chksum_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     sizeof(uint32_t),
                                     (uint8_t *)buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( proceed ) {

            p = (uint8_t *)buf;

            UINT32DECODE(p, chksum)

            *chksum_ptr = chksum;
            *new_offset_ptr = offset + (off_t)(sizeof(uint32_t));

        } else {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    return(proceed);

} /* bjf__load_chksum() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_end_trans_msg()
 *
 * Purpose:     Read and validate the contents of the end transaction
 *		message from the supplied file, and set:
 *
 *			*msg_type_ptr to JF__END_TRANS_MSG, and
 *
 *			*trans_num_ptr to the transaction number listed
 *				in the message.
 *
 *			*new_offset_ptr to the offset of the byte just
 *				after the end transaction message.
 *
 *		The supplied offset is presumed to reference the first
 *		byte of the message after the signature and version.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_end_trans_msg(int fd,
                        off_t file_len,
                        off_t offset,
                        off_t * new_offset_ptr,
  	                int * msg_type_ptr,
                        int msg_ver,
                        uint64_t * trans_num_ptr,
                        unsigned verbosity)
{
    const char * fcn_name = "bjf__load_end_trans_msg()";
    hbool_t proceed = TRUE;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( trans_num_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        switch ( msg_ver ) {

	    case 0:
                *new_offset_ptr = offset;

		proceed = bjf__load_trans_num(fd,
                                              file_len,
                                              *new_offset_ptr,
                                              new_offset_ptr,
  		                              msg_type_ptr,
                                              trans_num_ptr,
                                              NULL,
                                              verbosity);

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    *msg_type_ptr = JF__END_TRANS_MSG;

                } else {

                    *msg_type_ptr = JF__INVALID_MSG;
                    *new_offset_ptr = offset;

                }
		break;

	    default:
                *msg_type_ptr = JF__INVALID_MSG;
                *new_offset_ptr = offset;

                if ( verbosity > 1 ) {

                    HDfprintf(stdout,
                       "Unknown end trans msg ver detected at offset 0x%llx.\n",
                       (unsigned long long)offset);
                }
                break;
        }
    }

    return(proceed);

} /* bjf__load_end_trans_msg() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_entry_body()
 *
 * Purpose:     Read the specifice number of bytes from the supplied file
 *		starting at the indicated offset, and copy them into the
 *		supplied buffer.
 *
 *		Set *new_offset_ptr to reference the first byte after the
 *		entry body.
 *
 *		If chksum_ptr isn't NULL, checksum the data copied into
 *		the buffer, and update *chksum_ptr accordingly.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_entry_body(int fd,
                    off_t file_len,
                    off_t offset,
                    off_t * new_offset_ptr,
  	            int * msg_type_ptr,
                    size_t body_length,
                    char * buf_ptr,
                    size_t buf_len,
                    uint32_t * chksum_ptr,
                    unsigned verbosity)
{
    const char * fcn_name = "bjf__load_entry_body()";
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( buf_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);

    } else if ( body_length > buf_len ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: body (%d bytes) too big for buffer (%d bytes).\n",
                  fcn_name, (int)body_length, (int)buf_len);
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     body_length,
                                     (uint8_t *)buf_ptr,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( ! proceed ) {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    if ( ( proceed ) && ( ! eof ) ) {

        *new_offset_ptr = offset + (off_t)body_length;

        if ( chksum_ptr != NULL ) {

            *chksum_ptr = H5_checksum_metadata((const void *)buf_ptr,
                                               body_length,
                                               *chksum_ptr);

        }
    }

    return(proceed);

} /* bjf__load_entry_body() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_eoa_msg()
 *
 * Purpose:     Read and validate the contents of the end of address space
 *		message from the supplied file, and set:
 *
 *			*msg_type_ptr to JF__EOA_MSG, and
 *
 *			*new_offset_ptr to the offset of the byte just
 *				after the end of address space message.
 *
 *		The supplied offset is presumed to reference the first
 *		byte of the message after the signature and version.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_eoa_msg(int fd,
                  off_t file_len,
                  off_t offset,
                  off_t * new_offset_ptr,
	          int * msg_type_ptr,
                  int msg_ver,
                  haddr_t * eoa_ptr,
                  int hdf5_offset_width,
                  unsigned verbosity)
{
    const char * fcn_name = "bjf__load_eoa_msg()";
    hbool_t proceed = TRUE;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( eoa_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        switch ( msg_ver ) {

	    case 0:

                *new_offset_ptr = offset;

                proceed = bjf__load_offset(fd,
                                           file_len,
                                           *new_offset_ptr,
                                           new_offset_ptr,
                                           msg_type_ptr,
                                           eoa_ptr,
                                           hdf5_offset_width,
                                           NULL,
                                           verbosity);

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    *msg_type_ptr = JF__EOA_MSG;

                } else {

                    *msg_type_ptr = JF__INVALID_MSG;

                }
		break;

	    default:
                *msg_type_ptr = JF__INVALID_MSG;
                *new_offset_ptr = offset;

                if ( verbosity > 1 ) {

                    HDfprintf(stdout,
                       "Unknown eoa msg ver detected at offset 0x%llx.\n",
                       (unsigned long long)offset);
                }
                break;
        }
    }

    return(proceed);

} /* bjf__load_eoa_msg() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_jnl_entry_msg()
 *
 * Purpose:     Read and validate the contents of the journal entry
 *		message from the supplied file, and set:
 *
 *		*msg_type_ptr to JF__JNL_ENTRY_MSG,
 *
 *		*trans_num_ptr to the transaction number listed
 *			in the message,
 *
 *		*trans_num_ptr to the transaction number listed
 *                     	in the message,
 *
 *		*hdf5_offset_ptr to the base address listed in the
 *			message,
 *
 *		*hdf5_length_ptr to the entry length listed in the message,
 *
 *		if buf_ptr is not NULL, and buf_len is less than
 *			or equal to the length found in the message,
 *			load the body of the journal entry into
 *			*buf_ptr.
 *
 *		*new_offset_ptr to reference the first byte after
 *			the end of the journal entry message.
 *
 *		The supplied offset is presumed to reference the first
 *		byte of the message after the signature and version.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_jnl_entry_msg(int fd,
                        off_t file_len,
                        off_t offset,
                        off_t * new_offset_ptr,
  		        int * msg_type_ptr,
                        int msg_ver,
                        uint64_t * trans_num_ptr,
                        haddr_t * hdf5_offset_ptr,
                        int hdf5_offset_width,
                        size_t * hdf5_length_ptr,
                        int hdf5_length_width,
                        char * buf_ptr,
                        size_t buf_len,
                        unsigned verbosity)
{
    const char * fcn_name = "bjf__load_jnl_entry_msg()";
    hbool_t proceed = TRUE;
    uint32_t chksum = 0;
    uint32_t expected_chksum;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( trans_num_ptr == NULL ) ||
         ( hdf5_offset_ptr == NULL ) ||
         ( hdf5_length_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        switch ( msg_ver ) {

	    case 0:
                *new_offset_ptr = offset;

		proceed = bjf__load_trans_num(fd,
                                              file_len,
                                              *new_offset_ptr,
                                              new_offset_ptr,
  		                              msg_type_ptr,
                                              trans_num_ptr,
                                              &chksum,
                                              verbosity);
                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    proceed = bjf__load_offset(fd,
                                               file_len,
                                               *new_offset_ptr,
                                               new_offset_ptr,
                                               msg_type_ptr,
                                               hdf5_offset_ptr,
                                               hdf5_offset_width,
                                               &chksum,
                                               verbosity);
                }

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

		    proceed = bjf__load_length(fd,
                                               file_len,
                                               *new_offset_ptr,
                                               new_offset_ptr,
                                               msg_type_ptr,
                                               hdf5_length_ptr,
                                               hdf5_length_width,
                                               &chksum,
                                               verbosity);
                }

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    if ( buf_ptr != NULL ) {

			proceed = bjf__load_entry_body(fd,
                                                       file_len,
                                                       *new_offset_ptr,
                                                       new_offset_ptr,
  	                                               msg_type_ptr,
                                                       *hdf5_length_ptr,
                                                       buf_ptr,
                                                       buf_len,
                                                       &chksum,
                                                       verbosity);
                    } else {

			proceed = bjf__chksum_entry_body(fd,
                                                         file_len,
                                                         *new_offset_ptr,
                                                         new_offset_ptr,
  	                                                 msg_type_ptr,
                                                         *hdf5_length_ptr,
                                                         &chksum,
                                                         verbosity);
                    }
                }

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

		    proceed = bjf__load_chksum(fd,
                                               file_len,
                                               *new_offset_ptr,
                                               new_offset_ptr,
                                               msg_type_ptr,
                                               &expected_chksum,
                                               verbosity);
                }

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) &&
                     ( chksum != expected_chksum ) ) {

                    proceed = FALSE;
                    *msg_type_ptr = JF__INVALID_MSG;

                    if ( verbosity > 0 ) {

                        HDfprintf(stdout, "Chksum mismatch in jnl entry msg.\n");
			HDfprintf(stdout, "\texpected chksum = 0x%x\n",
                                  expected_chksum);
			HDfprintf(stdout, "\tactual chksum   = 0x%x\n", chksum);
                    }

                    if ( verbosity > 1 ) {

			HDfprintf(stdout, "\ttrans num = %lld\n",
                                  (long long)(*trans_num_ptr));
			HDfprintf(stdout, "\toffset = 0x%llx\n",
                                  (unsigned long long)(*hdf5_offset_ptr));
			HDfprintf(stdout, "\tlength = 0x%llx\n",
                                  (unsigned long long)(*hdf5_length_ptr));
                    }
                }

                if ( ( proceed ) &&
                     ( *msg_type_ptr == JF__UNDEFINED_MSG ) ) {

                    *msg_type_ptr = JF__JNL_ENTRY_MSG;
                }
		break;

	    default:
                *msg_type_ptr = JF__INVALID_MSG;
                *new_offset_ptr = offset;

                if ( verbosity > 0 ) {

                    HDfprintf(stdout,
                       "Unknown jnl entry msg ver detected at offset 0x%llx.\n",
                       (unsigned long long)offset);
                }
                break;
        }
    }

    return(proceed);

} /* bjf__load_jnl_entry_msg() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_length()
 *
 * Purpose:     Load the length (i.e. hdf5 object length) from the supplied
 *		journal file at the specified offset.
 *
 *		If successful, set *length_ptr to the newly loaded HDF5
 *		object length and set *new_offset_ptr to reference the
 *		first byte in the journal file after the HDF5 object length.
 *		Also, if chksum_ptr is not NULL, compute the checksum on
 *		the on disc representation of the HDF5 file object length
 *		and update *chksum_ptr accordingly.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE if any error is
 *		detected,
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_length(int fd,
                 off_t file_len,
                 off_t offset,
                 off_t * new_offset_ptr,
                 int * msg_type_ptr,
                 size_t * hdf5_length_ptr,
                 int hdf5_length_width,
                 uint32_t * chksum_ptr,
                 unsigned verbosity)
{
    const char * fcn_name = "bjf__load_length()";
    char buf[10];
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;
    uint8_t * p;
    size_t hdf5_length;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( hdf5_length_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     (size_t)hdf5_length_width,
                                     (uint8_t *)buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( proceed ) {

            p = (uint8_t *)buf;

            switch ( hdf5_length_width ) {

                case 2:
                    UINT16DECODE(p, hdf5_length)
                        *hdf5_length_ptr = hdf5_length;
                        *new_offset_ptr = offset + 2;
                        break;

                    case 4:
                        UINT32DECODE(p, hdf5_length)
                        *hdf5_length_ptr = hdf5_length;
                        *new_offset_ptr = offset + 4;
                        break;

                    case 8:
                        UINT64DECODE(p, hdf5_length)
                        *hdf5_length_ptr = hdf5_length;
                        *new_offset_ptr = offset + 8;
                        break;

                     default:
                        proceed = FALSE;
                        *msg_type_ptr = JF__INVALID_MSG;
                        *new_offset_ptr = offset;

                        if ( verbosity > 2 ) {

                            HDfprintf(stdout, "%s: invalid hdf5_length_width.\n",
                                      fcn_name);
                        }
                        break;
            }
        } else {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    if ( ( proceed ) && ( ! eof ) && ( chksum_ptr != NULL ) ) {

        *chksum_ptr = H5_checksum_metadata((const void *)buf,
                                           (size_t)hdf5_length_width,
                                           *chksum_ptr);
    }

    return(proceed);

} /* bjf__load_length() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_next_msg()
 *
 * Purpose:     Read the message from the supplied file that starts at
 *		the supplied offset, and set *msg_type_ptr, *trans_num_ptr,
 *		*offset_ptr, *length_ptr, and *eoa_ptr as appropriate.
 *		If the message is a journal entry, buf_ptr is not NULL
 *		and buf_len is greater than or equal to the length of the
 *		entry body, load the body into *buf_ptr.
 *
 *		In all cases, set *new_offset_ptr to either the offset at
 *		which the next message in the journal file is expected to
 *		begin, or to the offset at which the invalid message was
 *		detected.
 *
 *		More specifically:
 *
 *		If the the message is invalid, set:
 *
 *			*msg_type_ptr to JF__INVALID_MSG
 *
 *			The targets of all other pointer parameters are
 *			undefined.
 *
 *
 *		If the message is a begin transaction message, set:
 *
 *			*msg_type_ptr to JF__BEGIN_TRANS_MSG, and
 *
 *			*trans_num_ptr to the transaction number listed
 *				in the message.
 *
 *			The targets of all other pointer parameters are
 *				undefined.
 *
 *
 *		If the message is a journal entry message, set:
 *
 *			*msg_type_ptr to JF__JNL_ENTRY_MSG,
 *
 *			*trans_num_ptr to the transaction number listed
 *                      	in the message,
 *
 *			*offset_ptr to the base address listed in the
 *				message,
 *
 *			*length_ptr to the entry length listed in the
 *				message,
 *
 *			if buf_ptr is not NULL, and buf_len is less than
 *				or equal to the length found in the message,
 *				load the body of the journal entry into
 *				*buf_ptr.
 *
 *			The targets of all other pointer parameters are
 *			undefined.
 *
 *
 *		If the message is an end transaction message, set:
 *
 *			*msg_type_ptr to JF__END_TRANS_MSG, and
 *
 *			*trans_num_ptr to the transaction number listed
 *				in the message.
 *
 *			The targets of all other pointer parameters are
 *				undefined.
 *
 *
 *		If the message is an EOA message, set:
 *
 *			*msg_type_ptr to JF__EOA_MSG, and
 *
 *			*eoa_ptr to the end of address space value listed
 *				in the message
 *
 *			The targets of all other pointer parameters are
 *			undefined.
 *
 *		In all cases, set *new_offset_ptr to either the offset at
 *		which the next message in the journal file is expected to
 *		begin, or to the offset at which the invalid message was
 *		detected.
 *
 *		Return TRUE if successful, and FALSE otherwise.
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_next_msg(int fd,
                   off_t file_len,
                   off_t offset,
                   off_t * new_offset_ptr,
  		   int * msg_type_ptr,
                   uint64_t * trans_num_ptr,
                   haddr_t * hdf5_offset_ptr,
                   int hdf5_offset_width,
                   size_t * hdf5_length_ptr,
                   int hdf5_length_width,
                   haddr_t * eoa_ptr,
                   char * buf_ptr,
                   size_t buf_len,
                   unsigned verbosity)
{
    const char * fcn_name = "bjf__load_next_msg()";
    char sig_buf[H5C2_BJNL__SIG_LEN + 2];
    char msg_ver;
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( trans_num_ptr == NULL ) ||
         ( hdf5_offset_ptr == NULL ) ||
         ( hdf5_length_ptr == NULL ) ||
         ( eoa_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        *msg_type_ptr = JF__UNDEFINED_MSG;

        /* try to load the message signature and version into sig_buf */
        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     (size_t)(H5C2_BJNL__SIG_LEN + 1),
                                     (uint8_t *)sig_buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);
        if ( ! proceed ) {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    if ( ( proceed ) && ( ! eof ) ) {

        msg_ver = sig_buf[H5C2_BJNL__SIG_LEN];
        sig_buf[H5C2_BJNL__SIG_LEN] = '\0';

        if ( verbosity > 2 ) {

            HDfprintf(stdout,
                      "%s: Read sig = \"%s\", ver = %d at offset 0x%llx.\n",
                      fcn_name,
                      sig_buf,
                      (int)msg_ver,
                      (unsigned long long)(offset));
        }

        if ( HDstrncmp(sig_buf, H5C2_BJNL__BEGIN_TRANS_SIG,
                       (size_t)(H5C2_BJNL__SIG_LEN)) == 0 ) {

            proceed = bjf__load_begin_trans_msg(fd,
                                       file_len,
                                       offset + (off_t)(H5C2_BJNL__SIG_LEN + 1),
                                       new_offset_ptr,
                                       msg_type_ptr,
                                       msg_ver,
                                       trans_num_ptr,
                                       verbosity);

        } else if ( HDstrncmp(sig_buf, H5C2_BJNL__JOURNAL_ENTRY_SIG,
                              (size_t)(H5C2_BJNL__SIG_LEN)) == 0 ) {

	    proceed = bjf__load_jnl_entry_msg(fd,
                                       file_len,
                                       offset + (off_t)(H5C2_BJNL__SIG_LEN + 1),
                                       new_offset_ptr,
  		                       msg_type_ptr,
                                       msg_ver,
                                       trans_num_ptr,
                                       hdf5_offset_ptr,
                                       hdf5_offset_width,
                                       hdf5_length_ptr,
                                       hdf5_length_width,
                                       buf_ptr,
                                       buf_len,
                                       verbosity);

        } else if ( HDstrncmp(sig_buf, H5C2_BJNL__END_TRANS_SIG,
                              (size_t)(H5C2_BJNL__SIG_LEN)) == 0 ) {

            proceed = bjf__load_end_trans_msg(fd,
                                       file_len,
                                       offset + (off_t)(H5C2_BJNL__SIG_LEN + 1),
                                       new_offset_ptr,
                                       msg_type_ptr,
                                       msg_ver,
                                       trans_num_ptr,
                                       verbosity);


        } else if ( HDstrncmp(sig_buf, H5C2_BJNL__END_ADDR_SPACE_SIG,
                              (size_t)(H5C2_BJNL__SIG_LEN)) == 0 ) {

	    proceed = bjf__load_eoa_msg(fd,
                                       file_len,
                                       offset + (off_t)(H5C2_BJNL__SIG_LEN + 1),
                                       new_offset_ptr,
	                               msg_type_ptr,
                                       msg_ver,
                                       eoa_ptr,
                                       hdf5_offset_width,
                                       verbosity);

        } else {

            proceed = FALSE;
            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( verbosity > 0 ) {

                HDfprintf(stdout,
                  "Invalid/unknown binary msg sig detected at offset 0x%llx.\n",
                  (unsigned long long)offset);
            }
        }
    }

    return(proceed);

} /* bjf__load_next_msg() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_offset()
 *
 * Purpose:     Load the offset (i.e. hdf5 file address) from the supplied
 *		journal file at the specified offset.
 *
 *		If successful, set *hdf5_offset_ptr to the newly loaded HDF5
 *		file address and set *new_offset_ptr to reference the
 *		first byte in the journal file after the HDF5 file address.
 *		Also, if chksum_ptr is not NULL, compute the checksum on
 *		the on disc representation of the HDF5 file offset number,
 *		and update *chksum_ptr accordingly.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE if any error is
 *		detected,
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_offset(int fd,
                 off_t file_len,
                 off_t offset,
                 off_t * new_offset_ptr,
                 int * msg_type_ptr,
                 haddr_t * hdf5_offset_ptr,
                 int hdf5_offset_width,
                 uint32_t * chksum_ptr,
                 unsigned verbosity)
{
    const char * fcn_name = "bjf__load_offset()";
    char buf[10];
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;
    uint8_t * p;
    haddr_t hdf5_offset;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( hdf5_offset_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     (size_t)hdf5_offset_width,
                                     (uint8_t *)buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( proceed ) {

            p = (uint8_t *)buf;

            switch ( hdf5_offset_width ) {

                case 2:
                    UINT16DECODE(p, hdf5_offset)
                        *hdf5_offset_ptr = hdf5_offset;
                        *new_offset_ptr = offset + 2;
                        break;

                    case 4:
                        UINT32DECODE(p, hdf5_offset)
                        *hdf5_offset_ptr = hdf5_offset;
                        *new_offset_ptr = offset + 4;
                        break;

                    case 8:
                        UINT64DECODE(p, hdf5_offset)
                        *hdf5_offset_ptr = hdf5_offset;
                        *new_offset_ptr = offset + 8;
                        break;

                     default:
                        proceed = FALSE;
                        *msg_type_ptr = JF__INVALID_MSG;
                        *new_offset_ptr = offset;

                        if ( verbosity > 1 ) {

                            HDfprintf(stdout, "%s: invalid offset_width.\n",
                                      fcn_name);
                        }
                        break;
            }
        } else {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    if ( ( proceed ) && ( ! eof ) && ( chksum_ptr != NULL ) ) {

        *chksum_ptr = H5_checksum_metadata((const void *)buf,
                                           (size_t)hdf5_offset_width,
                                           *chksum_ptr);
    }

    return(proceed);

} /* bjf__load_offset() */


/*-------------------------------------------------------------------------
 * Function:    bjf__load_trans_num()
 *
 * Purpose:     Load the transaction number from the supplied file at the
 *		specified offset.
 *
 *		If successful, set *trans_num_ptr to the newly loaded
 *		transaction number, and set *new_offset_ptr to reference the
 *		first byte in the file after the transaction.  Also, if
 *		chksum_ptr is not NULL, compute the checksum on the on disc
 *		representation of the transaction number, and update *
 *		chksum_ptr accordingly.
 *
 *		If an error is detected, set *msg_type_ptr to
 *		JF__INVALID_MSG, and set *new_offset_ptr to the offset
 *		at which the error was detected.
 *
 *		Return TRUE if successful, and FALSE if any error is
 *		detected,
 *
 * Return:      TRUE on success
 *		FALSE otherwise
 *
 * Programmer:  John Mainzer -- 5/15/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
bjf__load_trans_num(int fd,
                    off_t file_len,
                    off_t offset,
                    off_t * new_offset_ptr,
  		    int * msg_type_ptr,
                    uint64_t * trans_num_ptr,
                    uint32_t * chksum_ptr,
                    unsigned verbosity)
{
    const char * fcn_name = "bjf__load_trans_num()";
    char buf[sizeof(uint64_t) + 1];
    hbool_t eof = FALSE;
    hbool_t proceed = TRUE;
    uint8_t * p;
    uint64_t trans_num;

    if ( ( fd < 0 ) ||
         ( new_offset_ptr == NULL ) ||
         ( msg_type_ptr == NULL ) ||
         ( *msg_type_ptr != JF__UNDEFINED_MSG ) ||
         ( trans_num_ptr == NULL ) ) {

        proceed = FALSE;
        HDfprintf(stderr, "%s: Bad parameters on entry.\n", fcn_name);
    }

    if ( proceed ) {

        proceed = load_buf_from_file(fd,
                                     file_len,
                                     offset,
                                     sizeof(uint64_t),
                                     (uint8_t *)buf,
                                     verbosity,
                                     fcn_name,
                                     &eof);

        if ( proceed ) {

            p = (uint8_t *)buf;

            UINT64DECODE(p, trans_num)

            *trans_num_ptr = trans_num;

            *new_offset_ptr = offset + (off_t)(sizeof(uint64_t));

            if ( chksum_ptr != NULL ) {

                *chksum_ptr = H5_checksum_metadata((const void *)buf,
                                                   sizeof(uint64_t),
                                                   *chksum_ptr);
            }

        } else {

            *msg_type_ptr = JF__INVALID_MSG;
            *new_offset_ptr = offset;

            if ( eof ) {

                /* we have encountered the end of file, not a system
                 * or I/O error.  Set proceed back to TRUE, as encountering
                 * the end of file is not necessarily an error in this
                 * context.
                 */

                proceed = TRUE;
            }
        }
    }

    return(proceed);

} /* bjf__load_trans_num() */


/*-------------------------------------------------------------------------
 * Function:     usage()
 *
 * Purpose:      Prints a usage message and then returns.
 *
 * Return:       void
 *
 * Programmer:   Mike McGreevy <mamcgree@hdfgroup.org>
 *               Monday, March 10, 2008
 *
 *-------------------------------------------------------------------------
 */
static void
usage (void)
{
 fprintf(stdout, "\
usage: h5recover [OPTIONS] [OBJECTS] [HDF5_FILE]\n\
   OBJECTS\n\
      -j, --journal [JOURNAL_FILE]      journal file name\n\
   OPTIONS\n\
      -b, --backup [BACKUP_NAME]        Specify a name for the backup copy\n\
                                            of the HDF5 file.\n\
                                            default = '[HDF5_FILE].backup'\n\
      -f  --force                       Recover without confirmation if the\n\
                                            journal file is empty.\n\
      -n, --nocopy                      Do not create a backup copy.\n\
      -h, --help                        Print a usage message and exit\n\
      -v, --verbose                     Generate more verbose output\n\
                                            (repeat for increased verbosity)\n\
      -V, --version                     Print version number and exit\n\
      -x, --examine                     Examine the supplied file(s), report,\n\
                                            and exit without action.\n");
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI & HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer:  Quincey Koziol
 *              Saturday, 31. January 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
 h5tools_close();

 exit(ret);
}

/*-------------------------------------------------------------------------
 * Function:     file_copy()
 *
 * Purpose:      Makes a back up copy of a file.
 *
 * Return:       0 on success
 *
 * Programmer:   Mike McGreevy <mamcgree@hdfgroup.org>
 *               Monday, March 10, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
file_copy(char * file_from, char * file_to)
{

    FILE * from;
    FILE * to;
    char ch;

    /* open source file */
    if((from = fopen(file_from, "rb")) == NULL) {
        error_msg(progname, "Could not open source file\n");
        leave( EXIT_FAILURE );
    }

    /* open destination file */
    if((to = fopen(file_to, "wb")) == NULL) {
        error_msg(progname, "Could not open destination file\n");
        leave( EXIT_FAILURE );
    }

    /* copy the file */
    while( !feof(from) ) {

        ch = fgetc(from);

        if( ferror(from) ) {
            error_msg(progname, "Error reading source file\n");
            leave( EXIT_FAILURE );
        } /* end if */

        if( !feof(from) )
            fputc(ch, to);

        if( ferror(to) ) {
            error_msg(progname, "Error writing destination file\n");
            leave( EXIT_FAILURE );
        } /* end if */

    } /* end while */

    /* close source file */
    if( fclose(from) == EOF ) {
        error_msg(progname, "Error closing source file\n");
        leave( EXIT_FAILURE );
    } /* end if */

    /* close destination file */
    if( fclose(to) == EOF ) {
        error_msg(progname, "Error closing destination file\n");
        leave( EXIT_FAILURE );
    } /* end if */

    return 0;

}

/*-------------------------------------------------------------------------
 * Function:     address_decode
 *
 * Purpose:      Decodes an address from the buffer pointed to by *pp and
 *               updates the pointer to point to the next byte after the
 *               address.
 *
 *               If the value read is all 1's, then the address is
 *               returned with an undefined value.
 *
 * Programmer:   Mike McGreevy (utilizing code from Robb Matzke)
 *               Thursday, August 7, 2008
 *
 *-------------------------------------------------------------------------
 */
void
address_decode(size_t sizeof_addr,
               const uint8_t **pp /* in, out */,
               haddr_t *addr_p/*out*/)
{

    unsigned i;
    haddr_t tmp;
    uint8_t c;
    hbool_t all_zero = TRUE;

    assert(pp && *pp);
    assert(addr_p);

    *addr_p = 0;

    for (i=0; i<sizeof_addr; i++) {
        c = *(*pp)++;
        if (c != 0xff)
            all_zero = FALSE;

        if (i<sizeof(*addr_p)) {
            tmp = c;
            tmp <<= (i * 8);    /*use tmp to get casting right */
            *addr_p |= tmp;
        } else if (!all_zero) {
            assert(0 == **pp);  /*overflow */
        }
    }

    if (all_zero)
        *addr_p = HADDR_UNDEF;

    return;

} /* address_decode() */

/*-------------------------------------------------------------------------
 * Function:    address_encode
 *
 * Purpose:     Encodes an address into the buffer pointed to by *pp and
 *              then increments the pointer to the first byte after the
 *              address. An undefined value is stored as all 1's.
 *
 * Programmer:  Mike McGreevy (utilizing code from Robb Matzke)
 *              Thursday, August 7, 2008
 *
 *-------------------------------------------------------------------------
 */
void
address_encode(size_t sizeof_addr,
               uint8_t **pp /* in, out */,
               haddr_t addr)
{
    unsigned u;

    assert(pp && *pp);

    for(u = 0; u < sizeof_addr; u++) {
        *(*pp)++ = (uint8_t)(addr & 0xff);
        addr >>= 8;
    } /* end for */

    assert("overflow" && 0 == addr);

    return;

} /* address_encode() */


/*-------------------------------------------------------------------------
 * Function:     length_decode
 *
 * Purpose:      Decodes a length from the buffer pointed to by *pp and
 *               updates the pointer to point to the next byte after the
 *               length.
 *
 * Programmer:   JRM -- 3/6/09 (rework of address_decode())
 *
 *-------------------------------------------------------------------------
 */
void
length_decode(size_t sizeof_length,
              const uint8_t **pp /* in, out */,
              hsize_t *len_p /*out*/)
{

    unsigned i;
    hsize_t tmp;
    uint8_t c;

    HDassert( pp && *pp );
    HDassert( len_p );
    HDassert( sizeof_length <= sizeof(hsize_t) );

    *len_p = 0;

    for ( i = 0; i < sizeof_length; i++ ) {

        c = *(*pp)++;

        if ( i < sizeof(*len_p)) {

            tmp = c;
            tmp <<= (i * 8);    /*use tmp to get casting right */
            *len_p |= tmp;
        }
    }

    return;

} /* length_decode() */

/*-------------------------------------------------------------------------
 * Function:    length_encode
 *
 * Purpose:     Encodes a length into the buffer pointed to by *pp and
 *              then increments the pointer to the first byte after the
 *              length.
 *
 * Programmer:  JRM -- 3/6/09 (rework of address_encode())
 *
 *-------------------------------------------------------------------------
 */

void
length_encode(size_t sizeof_length,
              uint8_t **pp /* in, out */,
              hsize_t length)
{
    unsigned u;

    HDassert(pp && *pp);
    HDassert( sizeof_length <= sizeof(hsize_t) );

    for ( u = 0; u < sizeof_length; u++ ) {

        *(*pp)++ = (uint8_t)(length & 0xff);
        length >>= 8;

    } /* end for */

    assert("overflow" && 0 == length);

    return;

} /* length_encode() */






/*-------------------------------------------------------------------------
 * Function:     main()
 *
 * Purpose:      main h5recover program.
 *
 * Programmer:   Mike McGreevy <mamcgree@hdfgroup.org>
 *               Monday, March 10, 2008
 *
 * Modifications:
 *               Mike McGreevy, August 7, 2008
 *               Parses superblock to update EOA value
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc,
     const char *argv[])
{

    /* ===================== */
    /* Variable Declarations */
    /* ===================== */

    hid_t            fid = -1; /* file id number */
    hid_t            fapl = -1; /* file access property list */
    char *           file_name = NULL; /* file name */
    off_t	     hdf5_file_len;
    off_t	     hdf5_sb_offset;
    int	 	     hdf5_sb_ver;
    haddr_t	     hdf5_base_addr;
    char *           file_name_backup = NULL; /* name of backup file */
    char *           journal_name = NULL; /* journal name */
    off_t            jnl_file_len;
    size_t           jnl_header_len;
    hbool_t          jnl_human_readable = TRUE;
    int              offset_width;
    int              length_width;
    unsigned         verbose = 0; /* verbose boolean */
    int              opt; /* option number */
    FILE *           journal_fp; /* journal file pointer */
    int              hdf5_fd; /* hdf5 file descriptor */
    char *           readback;   /* array to read into from journal */
    char *           last_trans_msg; /* array to keep last end_trans msg */
    int              last_trans_found = 0; /* bool */
    char *           tok[11];     /* string tokens */
    int              i; /* iterator */
    off_t            address; /* address to write to */
    haddr_t          eoa = 0; /* end of address of file */
    haddr_t          update_eoa = 0; /* new end of address */
    uint8_t *        body; /* body of journal entry */
    size_t           size; /* size of journal entry body */
    size_t           max_size; /* maximum size of journal entry body */
    char *           p; /* pointer */
    hbool_t          custom_name = 0; /* bool indicating custom backup name */
    hbool_t          no_copy = 0; /* bool indicating not to make backup */
    H5AC2_jnl_config_t config; /* journaling configuration */
    hbool_t          check_file = 1; /* boolean indicating whether to check */
    uint8_t *        compare_buf; /* buffer to read into from hdf5 file */
    hid_t            status = -1; /* status indicator for retval checking */
    char             c_old,c_new; /* temporary storage for single characters */
    long             pos; /* file descriptor position indicator */
    long             pos_end; /* file descriptor position indicator */
    char             temp[100]; /* temporary buffer */
    H5F_t *          f; /* File pointer */
    hbool_t          jrnl_has_transactions = TRUE;
    hbool_t          examine_files_only = FALSE;
    hbool_t          force = FALSE;
    hbool_t          error = FALSE;

    /* ================================================ */
    /* Variables needed for superblock parse and update */
    /* ================================================ */

    int              n; /* iterator */
    haddr_t          sb_addr; /* address of superblock */
    uint8_t          buf[H5F_SIGNATURE_LEN]; /* buf to get superblock signature */
    uint8_t          sbuf[H5F_MAX_SUPERBLOCK_SIZE]; /* buf to store superblock */
    int              super_vers = 0; /* superblock version number */
    int              sizeof_addr = 0; /* size of addresses */
    haddr_t          addr = NULL; /* address buffer to hold haddr_t values */
    uint8_t *  p_front = NULL; /* reference pointer into superblock buffer */
    uint8_t *  p_end = NULL; /* reference pointer into superblock buffer */
    haddr_t          new_eoa; /* new value of EOA to be written into superblock */
    uint32_t         chksum; /* calculated checksum value */
    uint32_t         read_chksum; /* checksum value read from superblock */

    /* ==================== */
    /* Command Line Parsing */
    /* ==================== */

    /* initialize h5tools lib */
    h5tools_init();

    /* Check for no command line parameters */
    if ( argc == 1 ) {

        usage();
        leave( EXIT_SUCCESS );

    } /* end if */

    /* parse command line options */
    while ( (opt = get_option(argc, argv, s_opts, l_opts) ) != EOF) {

        switch ((char)opt) {

            case 'h':
                usage();
                leave( EXIT_SUCCESS );
                break;

            case 'b':
                custom_name = 1;
                file_name_backup = strdup ( opt_arg );
                break;

            case 'f':
                force = TRUE;
                break;

            case 'n':
                no_copy = 1;
                break;

            case 'j':
                journal_name = strdup( opt_arg );
                break;

            case 'V':
                print_version(progname);
                leave( EXIT_SUCCESS );
                break;

            case 'v':
                verbose++;
                break;

            case 'x':
                examine_files_only = 1;
                break;

            default:
                usage();
                leave( EXIT_FAILURE );

        } /* end switch */

    } /* end while */

    /* check for missing file name */
    if (argc <= opt_ind) {
        error_msg(progname, "HDF5 file name missing\n");
        usage();
        leave( EXIT_FAILURE );
    }

    file_name = HDstrdup(argv[opt_ind]);

    /* check for missing journal name */
    if ( journal_name == NULL ) {

        error_msg(progname, "Journal file name missing\n");
        usage();
        leave( EXIT_FAILURE );

    } /* end if */

    /* check for missing backup file name */
    if ( custom_name == 1) {

        if ( file_name_backup == NULL ) {

            error_msg(progname, "Journal file backup name missing\n");
            usage();
            leave( EXIT_FAILURE );

        } /* end if */

    } /* end if */

    /* if we are only to examine the files, do so now and exit */
    if ( examine_files_only ) {

        examine_files(file_name, journal_name, verbose);

        leave( EXIT_SUCCESS );
    }

    /* ================================================================ */
    /* Verify that the supplied HDF5 file is marked as being journaled, */
    /* and that the supplied journal file matches the HDF5 file.  Exit  */
    /* if this is not the case.                                         */
    /* ================================================================ */

    if ( ! verify_files(file_name,
                        &hdf5_file_len,
                        &hdf5_sb_offset,
                        &hdf5_sb_ver,
                        &hdf5_base_addr,
                        journal_name,
                        &jnl_file_len,
                        &jnl_header_len,
                        force,
                        &jnl_human_readable,
                        &offset_width,
                        &length_width,
                        verbose,
                        &error) ) {

        if ( error ) {

            leave( EXIT_FAILURE );

        } else {

            leave( EXIT_SUCCESS );
        }
    }


    /* =============================== */
    /* Make a Backup Copy of HDF5 File */
    /* =============================== */

    if ( verbose ) printf("\n==============================================\n");

    if (no_copy == 0) {

        /* make a backup copy of HDF5 file before recovery */
        if (custom_name == 1) {

            if ( verbose ) printf("Copying HDF5 file <%s> into file <%s>\n", file_name, file_name_backup);

            file_copy( /* from */ file_name, /* to */ file_name_backup);

        } /* end if */

        else {
            file_name_backup = HDmalloc(HDstrlen(file_name) + (size_t)10);

            HDsnprintf(file_name_backup, HDstrlen(file_name) + (size_t)8, "%s.backup", file_name);
            if ( verbose ) printf("Copying HDF5 file <%s> into file <%s>\n", file_name, file_name_backup);
            file_copy( /* from */ file_name, /* to */ file_name_backup);

        } /* end else */

    } /* end if */

    if ( jnl_human_readable ) {

        /* =========================== */
        /* Open HDF5 and Journal Files */
        /* =========================== */

        /* open the journal file for reading */
        journal_fp = fopen(journal_name, "r");

        if (journal_fp == NULL) {

            error_msg(progname, "Could not open specified journal file\n");
            usage();
            leave( EXIT_FAILURE );

        } /* end if */

        /* open hdf5 file for reading and writing */
        hdf5_fd = open(file_name, O_RDWR);

        if (hdf5_fd == -1) {

            error_msg(progname, "Could not open specified hdf5 file\n");
            usage();
            leave( EXIT_FAILURE );

        } /* end if */

        /* allocate temporary space to store and transaction messages */
        last_trans_msg = HDmalloc((size_t)50);
        if (last_trans_msg == NULL) {

            error_msg(progname, "Could not allocate space\n");
            leave( EXIT_FAILURE);

        } /* end if */

        if ( verbose ) printf("Recovering file <%s> from journal <%s>\n\n",
                              file_name, journal_name);

        /* ====================================================== */
        /* Find the last complete transaction in the journal file */
        /* ====================================================== */

        fseek(journal_fp, 0, SEEK_END);

        while (last_trans_found == 0) {

            while (fgetc(journal_fp) != '\n') {

                if (ftell(journal_fp) <= 1) {

                    jrnl_has_transactions = FALSE;
                    printf("Journal file has no complete transactions. Nothing to recover!\n");
                    break;

                } /* end if */

                fseek(journal_fp, -2, SEEK_CUR);

            } /* end while */

            if (jrnl_has_transactions == FALSE) break;

            if ( fgetc(journal_fp) == '3' ) {

                last_trans_found = 1;
                pos_end = ftell(journal_fp);
                fseek(journal_fp, -1, SEEK_CUR);
                fgets(last_trans_msg, 50, journal_fp);

            } /* end if */

            else {

                fseek(journal_fp, -3, SEEK_CUR);

            } /* end else */

        } /* end while */

        /* ================================================================== */
        /* Only do the recovery procedure if there is something to recover in */
        /* the journal file. Otherwise, skip over these steps and mark        */
        /* the file as recovered.                                             */
        /* ================================================================== */

        if (jrnl_has_transactions == TRUE) {

            /* ================================================================= */
            /* Pre-parse of journal file to pull information needed before doing */
            /* the recovery.                                                     */
            /*    - max journal size (for buffer allocation)                     */
            /*    - max EOA size (for superblock update, to preserve raw data)   */
            /* ================================================================= */

            fseek(journal_fp, 0, SEEK_SET);

            c_new = 0;
            c_old = 0;
            max_size = 0;

            if ( verbose ) printf("Pre-parsing journal file to pull needed data ... \n");

            /* while journal is not at end of file */
            while (ftell(journal_fp) != pos_end) {

                c_old = c_new;
                c_new = fgetc(journal_fp);

                /* if position is at the start of a line */
                if (c_old == '\n') {

                    /* ========================================================== */
                    /* if the line is a journal entry, determine its size. update */
                    /* max size value if needed.                                  */
                    /* ========================================================== */
                    if (c_new == '2') {

                        pos = ftell(journal_fp);

                        fgets(temp, 100, journal_fp);
                        tok[0] = HDstrtok(temp, " ");
                        if (tok[0] == NULL) {

                            error_msg(progname, "Could not tokenize entry\n");
                            leave( EXIT_FAILURE);

                        } /* end if */
                        for (i=1; i<8; i++) {

                            tok[i] = HDstrtok(NULL, " ");
                            if (tok[i] == NULL) {

                                error_msg(progname, "Could not tokenize entry\n");
                                leave( EXIT_FAILURE);

                            } /* end if */

                        } /* end for */

                        size = (size_t)HDstrtoll(tok[3], NULL, 10);

                        if (max_size < size) {

                            max_size = size;

                        } /* end if */

                        /* jump back to start of line */
                        fseek(journal_fp, pos, SEEK_SET);

                    } /* end if */

                    /* =========================================================== */
                    /* If the line is an EOA entry, determine its value and update */
                    /* if it exceeds the current max length                        */
                    /* =========================================================== */

                    if (c_new == 'E') {

                        pos = ftell(journal_fp);

                        fgets(temp, 100, journal_fp);
                        p = &temp[11];

    		    /* according to the man page, strtoll() should accept a
    		     * "0x" prefix on any base 16 value -- seems this is
    		     * not the case.  Deal with this by incrementing p
    		     * past the prefix.
    		     */

    		    while ( HDisspace(*(p)) ) { p++; }

    		    if ( ( *(p) == '0' ) &&
    		         ( *(p + 1) == 'x' ) ) {

    		            p += 2;
    		    }

                        eoa = (haddr_t)HDstrtoll(p, NULL, 16);
                        if (eoa == 0) {

                            error_msg(progname,
    				  "Could not convert eoa to integer\n");
                            leave( EXIT_FAILURE);

                        } /* end if */

                        if (update_eoa < eoa) {

                            update_eoa = eoa;

                        } /* end if */

                        /* jump back to start of line */
                        fseek(journal_fp, pos, SEEK_SET);

                    } /* end if */

                } /* end if */

            } /* end while */

            if ( verbose ) printf(" - Maximum journal entry size = %d\n", max_size);
            if ( verbose ) printf(" - Journaled EOA value = 0x%llx\n", update_eoa);

            /* =================================== */
            /* Update EOA value in HDF5 superblock */
            /* =================================== */

            if (update_eoa != 0) {

                if ( verbose ) printf("\nLooking for HDF5 superblock ... \n");
                /* Jump through possible locations of superblock */
                for(n = 8; n < 16; n++) {

                    sb_addr = (8 == n) ? 0 : 1 << n;

                    /* read from HDF5 file */
                    pread(hdf5_fd, buf, H5F_SIGNATURE_LEN, sb_addr);

                    /* Check to see if superblock has been found. */
                    if(!HDmemcmp(buf, H5F_SIGNATURE, (size_t)H5F_SIGNATURE_LEN)) {

                        if ( verbose ) printf(" - Superblock signature found at location %d\n", sb_addr);

                        if ( verbose ) printf(" - Reading in entire superblock\n");

                        /* Read in entire superblock */
                        pread(hdf5_fd, sbuf, H5F_MAX_SUPERBLOCK_SIZE, sb_addr/* + H5F_SIGNATURE_LEN */);

                        /* Use p as a pointer into superblock buffer */
                        p = sbuf;

                        /* Skip over signature */
                        p += H5F_SIGNATURE_LEN;

                        /* Get superblock version number */
                        super_vers = *p++;

                        /* add printfs to verbose */
                        if ( verbose ) printf(" - Superblock version number = %d\n", super_vers);

                        /* ==================================== */
                        /* Point to EOA value in the superblock */
                        /* ==================================== */

                        /* First part of superblock may be of differing versions */
                        if(super_vers < 2) {
                            /* skip over unneeded data */
                            p += 1 +  /* freespace version */
                                 1 +  /* root group version */
                                 1 +  /* reserved byte */
                                 1;   /* shared header version */

                            sizeof_addr = *p++; /* size of file addresses */

                            p += 1 +  /* size of file sizes */
                                 1 +  /* reserved byte */
                                 2 +  /* 1/2 rank for symtable leaf nodes */
                                 2 +  /* 1/2 rank for btree internal nodes */
                                 4 +  /* file status flags */
                                 2;   /* b-tree internal k value */

                            if (super_vers == 1)
                                p += 2; /* reserved bytes */

                        } /* end if */

                        /* Superblock version number > 2 */
                        else {

                            sizeof_addr = *p++; /* size of file addresses */

                            p += 1 + /* size of file sizes */
                                 1;  /* file status flags */

                            p_front = p;
                            p_end = p;

                        } /* end else */

                        /* Skip over various variable portions of superblock */
                        address_decode((size_t)sizeof_addr, &p_end, &addr); /* base address */
                        address_decode((size_t)sizeof_addr, &p_end, &addr); /* extension address */

                        /* ============================ */
                        /* Update EOA in the superblock */
                        /* ============================ */

                        p_front = p_end;

                        /* Decode the EOA address */
                        address_decode((size_t)sizeof_addr, &p_end, &addr);

                        if ( verbose ) printf(" - Current value of EOA in superblock is 0x%llx\n", addr);

                        /* Set the EOA to the address pulled from the journal */
                        addr = (haddr_t)update_eoa;
                        p_end = p_front;

                        /* encode new EOA value into superblock buffer */
                        address_encode((size_t)sizeof_addr, &p_end, addr);

                        if ( verbose ) printf(" - EOA value has been updated to 0x%llx in superblock\n", update_eoa);

                        /* skip over root group object header */
                        address_decode((size_t)sizeof_addr, &p_end, &addr); /* root group object header */

                        p_front = p_end;

                        if ( verbose ) printf(" - Updating checksum value of superblock\n");

                        /* decode checksum */
                        read_chksum = 0;
                        UINT32DECODE(p_end, read_chksum);

                        p_end = p_front;

                        /* Update the CHECKSUM VALUE */
                        /* Compute superblock checksum */
                        chksum = H5_checksum_metadata(sbuf, (size_t)(p_end - sbuf), 0);

                        /* Superblock checksum */
                        UINT32ENCODE(p_end, chksum);

                        new_eoa = (haddr_t)update_eoa;

                        /* verify new EOA value in buffer is correct */
                        address_decode((size_t)sizeof_addr, &p_front, &addr);

                        /* Extend file to be EOA bytes */
                        HDftruncate(hdf5_fd, new_eoa);

                        /* Write out new updated superblock to the file */
                        status = pwrite(hdf5_fd, sbuf, (size_t)H5F_MAX_SUPERBLOCK_SIZE, sb_addr /*+ H5F_SIGNATURE_LEN */);

                        if (status == -1) {
                            error_msg(progname, "pwrite failed when trying to update superblock\n");
                            leave( EXIT_FAILURE );
                        }

                        if (status == 0) {
                            error_msg(progname, "pwrite did not write anything to superblock!\n");
                            leave( EXIT_FAILURE);
                        }

                        if ( verbose ) printf(" - New superblock written to HDF5 file\n");

                    } /* end if */

                } /* end for */

            } /* end if (update_eoa != 0)*/

            if ( verbose ) printf("\nBeginning recovery process ... \n\n");

            /* ==================================================================== */
            /* Main Recovery Procedure:                                             */
            /* Read through the journal file and recover any journal entries found. */
            /* ==================================================================== */

            max_size = max_size * 3 + 200;

            /* allocate space large enough to hold largest journal entry */
            readback = HDmalloc( max_size );
            if (readback == NULL) {

                error_msg(progname, "Could not allocate space to hold entries\n");
                leave( EXIT_FAILURE);

            } /* end if */

            /* read through journal file. recover any journal entries found, up
             * through the last transaction number */
            fseek(journal_fp, 0, SEEK_SET);

            while ( fgets(readback, max_size, journal_fp) != NULL ) {

                if (HDstrcmp(readback, last_trans_msg) == 0) {

                    /* done reading from file */
                    break;

                } /* end if */

                /* ===================================================== */
                /* If journal entry is found, write entry into HDF5 file */
                /* ===================================================== */

                if ( readback[0] == '2') { /* journal entry found */

                    if ( verbose ) printf("Journal entry found.\n");
                    if ( verbose ) printf("Tokenizing journal entry.\n");

                    /* ================================================= */
                    /* Tokenize the journal entry in order to grab data */
                    /* ================================================= */

                    /* divide the journal entry into tokens */
                    tok[0] = HDstrtok(readback, " ");
                    if (tok[0] == NULL) {

                        error_msg(progname, "Could not tokenize journal entry\n");
                        leave( EXIT_FAILURE);

                    } /* end if */

                    if ( verbose ) printf("  token[0] : <%s>\n", tok[0]);

                    for (i=1; i<8; i++) {

                        tok[i] = HDstrtok(NULL, " ");
                        if (tok[i] == NULL) {

                            error_msg(progname, "Could not tokenize journal entry\n");
                            leave( EXIT_FAILURE);

                        } /* end if */

                        if ( verbose ) printf("  token[%d] : <%s>\n", i, tok[i]);

                    } /* end for */

                    /* put all remaining data into last token. */
                    /* This contains all of the journal entry body */
                    tok[8] = HDstrtok(NULL, "\n");
                    if (tok[8] == NULL) {

                        error_msg(progname, "Could not tokenize journal entry\n");
                        leave( EXIT_FAILURE);

                    } /* end if */

                    if ( verbose ) printf("  token[8] : <hexadecimal body data>\n");

                    /* =================================== */
                    /* Convert Items from Character Arrays */
                    /* =================================== */

                    if ( verbose ) printf("Converting data from character strings.\n");

                    /* convert address from character character string */

    		/* according to the man page, strtoll() should accept a
    		 * "0x" prefix on any base 16 value -- seems this is
    		 * not the case.  Deal with this by incrementing tok[6]
    		 * past the prefix.
    		 */
    		while ( HDisspace(*(tok[6])) ) { (tok[6])++; }
    		if ( ( *(tok[6]) == '0' ) &&
    		     ( *(tok[6] + 1) == 'x' ) ) {

    		        (tok[6]) += 2;
    		}
                    address = (off_t)HDstrtoll(tok[6], NULL, 16);
                    if (address == 0) {

                        error_msg(progname, "Could not convert address to integer\n");
                        leave( EXIT_FAILURE);

                    } /* end if */

                    if ( verbose ) printf("  address  : %llx\n", address);

                    /* convert size from character string*/
                    size = (size_t)HDstrtoll(tok[4], NULL, 10);
                    if (size == 0) {

                        error_msg(progname, "Could not convert size to double\n");
                        leave( EXIT_FAILURE);

                    } /* end if */

                    if ( verbose ) printf("  length   : %d\n", size);

                    /* transform body out of hexadecimal character string */
                    body = HDmalloc(size + 1);
                    if (body == NULL) {

                        error_msg(progname, "Could not allocate space for body\n");
                        leave( EXIT_FAILURE);

                    } /* end if */

                    p = &(tok[8])[0];

                    for (i = 0; i < size; i++) {

                        body[i] = HDstrtoul(p, NULL, 16);
                        p = &p[3];

                    } /* end for */

                    body[i] = 0;

                    if ( verbose ) printf("  body     : binary body data\n");

                    /* ================================================ */
                    /* Write into HDF5 file the recovered journal entry */
                    /* ================================================ */

                    if ( verbose ) printf("Writing entry to HDF5 file.\n");

                    /* perform a write */
                    status = pwrite(hdf5_fd, body, size, address);

                    if (status == -1) {
                        error_msg(progname, "pwrite failed\n");
                        leave( EXIT_FAILURE );
                    }

                    if (status == 0) {
                        error_msg(progname, "pwrite did not write anything!\n");
                        leave( EXIT_FAILURE);
                    }

                    /* Verify that write occurred correctly */
                    if ( check_file == 1) {

                        if ( verbose ) printf("Verifying success of write");

                        compare_buf = HDmalloc(size + 1);

                        if (compare_buf == NULL) {
                            error_msg(progname, "Could not allocate space\n");
                            leave( EXIT_FAILURE);
                        } /* end if */

                        pread(hdf5_fd, compare_buf, size, address);

                        /* do a quick string compare on two items */
                        if (HDstrcmp((const char *)body, (const char *)compare_buf) != 0) {
                            error_msg(progname, "Entry incorrectly written into HDF5 file. Exiting.\n");
                            printf("Address %llx:\n", (unsigned long_long)address);
                            printf(" -- from journal:   '%s'\n", body);
                            printf(" -- from HDF5 file: '%s'\n", compare_buf);
                            leave( EXIT_FAILURE );
                        } /* end if */

                        /* compare each individual value of entry */
                        for (i=0; i<size; i++) {
                            if (body[i] != compare_buf[i]) {
                                error_msg(progname, "Entry incorrectly written into HDF5 file. Exiting.\n");
                                printf("Address %llx\n", (unsigned long_long)(address + i));
                                printf(" -- from journal:   %d\n", body[i]);
                                printf(" -- from HDF5 file: %d\n", compare_buf[i]);
                                leave( EXIT_FAILURE );
                            }
                        }

                        if ( verbose ) printf(" .... SUCCESS!\n\n");
                        free(compare_buf);

                    }

                    free(body);

                } /* end if */

            } /* end while */

            free(readback);

        } /* end if jrnl_has_transactions */

        fclose(journal_fp);
        close(hdf5_fd);
        free(last_trans_msg);
        free(journal_name);
        if (file_name_backup != NULL) HDfree(file_name_backup);

        /* =========================== */
        /* Mark HDF5 File as Recovered */
        /* =========================== */

        /* set up appropriate fapl */
        fapl = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl == -1 ) {

            error_msg(progname, "Could not create FAPL.\n");
            leave( EXIT_FAILURE );

        } /* end if */

        config.version = 1; /* should be H5C2__CURR_AUTO_SIZE_CTL_VER */

        /* get H5AC_cache_config_t configuration from fapl */
        if ( H5Pget_jnl_config(fapl, &config) == -1) {

            error_msg(progname, "Could not get mdc config from FAPL.\n");
            leave( EXIT_FAILURE );

        }

        /* make sure journal recovered field is set to TRUE in mdc_config */
        config.journal_recovered = TRUE;

        /* set H5AC_cache_config_t configuration with file recovered */
        if ( H5Pset_jnl_config(fapl, &config) == -1) {

            error_msg(progname, "Could not set mdc config on FAPL.\n");
            leave( EXIT_FAILURE );

        } /* end if */

        /* open HDF5 file with provided fapl */
        fid = H5Fopen(file_name, H5F_ACC_RDWR, fapl);

        if ( fid == -1 ) {

            error_msg(progname, "Could not open recovered HDF5 file.\n");
            leave( EXIT_FAILURE );

        } /* end if */

        /* close HDF5 file */
        if ( H5Fclose(fid) == -1 ) {

            error_msg(progname, "Could not close recovered HDF5 file.\n");
            leave( EXIT_FAILURE );

        } /* end if */

        /* ================ */
        /* Cleanup and Exit */
        /* ================ */

        if (jrnl_has_transactions == TRUE)
            printf("HDF5 file successfuly recovered.\n");
        else
            printf("File marked as recovered.\n");

        if ( verbose ) printf("==============================================\n\n");
    } else {

        hbool_t proceed = TRUE;
        uint64_t max_trans_num = 0;
        haddr_t max_eoa = HADDR_UNDEF;
        size_t max_body_len = 0;

        proceed = scan_journal_file(journal_name,
                                    jnl_file_len,
                                    jnl_header_len,
                                    jnl_human_readable,
                                    offset_width,
                                    length_width,
                                    &max_trans_num,
                                    &max_eoa,
                                    &max_body_len,
                                    verbose);

        if ( ( proceed ) && ( max_trans_num > 0 ) ) {

            proceed = apply_journal_file(file_name,
                                         hdf5_file_len,
                                         hdf5_sb_offset,
                                         hdf5_sb_ver,
                                         hdf5_base_addr,
                                         journal_name,
                                         jnl_file_len,
                                         jnl_header_len,
                                         jnl_human_readable,
                                         offset_width,
                                         length_width,
                                         max_trans_num,
                                         max_eoa,
                                         max_body_len,
                                         verbose);
        }

        if ( proceed ) {

            proceed = mark_hdf5_file_recovered(file_name);
        }
    }

    free(file_name);

    return 0;

} /* end main */

