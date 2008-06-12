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

#define H5F_PACKAGE

#include "hdf5.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "H5private.h"
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "H5Fpkg.h"

const char *    progname="h5recover";
int             d_status;

/* Define valid command line options. */
static const char *s_opts = "hb:j:vnV";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "backup", require_arg, 'b' },
    { "journal", require_arg, 'j' },
    { "verbose", no_arg, 'v' },
    { "nocopy", no_arg, 'n' },
    { "version", no_arg, 'V' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function:     usage()
 *
 * Purpose:      Prints a usage message and then returns.
 *
 * Return:       void
 *
 * Programmer:   Mike McGreevy <mcgreevy@hdfgroup.org>
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
      -n, --nocopy                      Do not create a backup copy.\n\
      -h, --help                        Print a usage message and exit\n\
      -v, --verbose                     Generate more verbose output\n\
      -V, --version                     Print version number and exit\n");
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
 * Programmer:   Mike McGreevy <mcgreevy@hdfgroup.org>
 *               Monday, March 10, 2008
 *
 *-------------------------------------------------------------------------
 */
int 
file_copy(char * file_from, char * file_to){

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
 * Function:     main()
 *
 * Purpose:      main h5recover program.
 *
 * Programmer:   Mike McGreevy <mcgreevy@hdfgroup.org>
 *               Monday, March 10, 2008
 *
 *-------------------------------------------------------------------------
 */

int
main (int argc, const char *argv[])
{

    /***********************
    * Variable declarations
    ***********************/    

    hid_t            fid = -1; /* file id number */
    hid_t            fapl = -1; /* file access property list */
    char *           file_name = NULL; /* file name */
    char *           file_name_backup = NULL; /* name of backup file */
    char *           journal_name = NULL; /* journal name */
    unsigned         verbose = 0; /* verbose boolean */
    int              opt; /* option number */
    FILE *           journal_fp; /* journal file pointer */
    int              hdf5_fd; /* hdf5 file descriptor */
    char *           readback;   /* array to read into from journal */
    char *           last_trans_msg; /* array to keep last end_trans msg */
    int              last_trans_found = 0; /* bool */
    char *           tok[11];     /* string tokens */
    int              i; /* iterator */
    haddr_t          address; /* address to write to */
    haddr_t          eoa; /* end of address of file */
    uint8_t *        body; /* body of journal entry */
    size_t           size; /* size of journal entry body */
    size_t           max_size; /* maximum size of journal entry body */
    char *           p; /* pointer */
    hbool_t          custom_name = 0; /* bool indicating custom backup name */
    hbool_t          no_copy = 0; /* bool indicating not to make backup */
    H5AC_cache_config_t config; /* cache configuration */
    hbool_t          check_file = 1; /* boolean indicating whether to check */
    uint8_t *        compare_buf; /* buffer to read into from hdf5 file */
    hid_t            status = -1; /* status indicator for retval checking */
    char             c_old,c_new; /* temporary storage for single characters */
    long             pos; /* file descriptor position indicator */
    long             pos_end; /* file descriptor position indicator */
    char             temp[100]; /* temporary buffer */
    H5F_t *          f; /* File pointer */

    /**********************
    * Command line parsing 
    **********************/

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
                verbose = 1;
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

    /* =================== */
    /* Main H5recover code */
    /* =================== */

 
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

    /* allocate temporary space to store end transaction messages */
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

                error_msg(progname, "Journal has no complete transactions\n");
                usage();
                leave( EXIT_FAILURE );
        
            } /* end if */

            fseek(journal_fp, -2, SEEK_CUR);

        } /* end while */

        if ( fgetc(journal_fp) == '3' ) {

            last_trans_found = 1;
            fseek(journal_fp, -1, SEEK_CUR);
            fgets(last_trans_msg, 50, journal_fp);

        } /* end if */

        else {

            fseek(journal_fp, -3, SEEK_CUR);

        } /* end else */

    } /* end while */

    /* ======================================= */
    /* determine size of biggest journal entry */
    /* ======================================= */
    fseek(journal_fp, 0, SEEK_END);
    pos_end = ftell(journal_fp);

    fseek(journal_fp, 0, SEEK_SET);
    
    c_new = 0;
    c_old = 0;
    max_size = 0;

    /* while journal is not at end of file */
    while (ftell(journal_fp) != pos_end) {

        c_old = c_new;
        c_new = fgetc(journal_fp);
        
        /* if position is at the start of a line */
        if (c_old == '\n') {

            /* if the line starts with a '2', find out size of entry */
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
                
                size = HDstrtod(tok[3], NULL);

                if (max_size < size) {

                    max_size = size;

                } /* end if */

                /* jump back to start of line */
                fseek(journal_fp, pos, SEEK_SET);

            } /* end if */

        } /* end if */

    } /* end while */

    /* ================================================= */
    /* Tokenize each journal entry in order to grab data */
    /* ================================================= */

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
    
        if ( readback[0] == '2') { /* journal entry found */

        if ( verbose ) printf("Journal entry found.\n");
        if ( verbose ) printf("Tokenizing journal entry.\n");

            /* divide the journal entry into tokens */
            tok[0] = HDstrtok(readback, " ");
            if (tok[0] == NULL) {

                error_msg(progname, "Could not tokenize journal entry\n");
                leave( EXIT_FAILURE);
    
            } /* end if */

            /* if ( verbose ) printf("  token[0] : <%s>\n", tok[0]); */

            for (i=1; i<10; i++) {

                tok[i] = HDstrtok(NULL, " ");
                if (tok[i] == NULL) {

                    error_msg(progname, "Could not tokenize journal entry\n");
                    leave( EXIT_FAILURE);

                } /* end if */

            /* if ( verbose ) printf("  token[%d] : <%s>\n", i, tok[i]); */

            } /* end for */

            /* put all remaining data into last token. */ 
            /* This contains all of the journal entry body */
            tok[10] = HDstrtok(NULL, "\n");
            if (tok[10] == NULL) {

                error_msg(progname, "Could not tokenize journal entry\n");
                leave( EXIT_FAILURE);

            } /* end if */

            /* if ( verbose ) printf("  token[8] : <%s>\n", tok[8]); */

            /* ================================== */
            /* Convert Items from Character Array */
            /* ================================== */

            if ( verbose ) printf("Converting data from character strings.\n");

            /* convert address from character character string */
            address = HDstrtod(tok[8], NULL);
            if (address == 0) {
    
                error_msg(progname, "Could not convert address to integer\n");
                leave( EXIT_FAILURE);

            } /* end if */

            /* if ( verbose ) printf("  address: %llx\n", address); */

            /* convert size from character string*/
            size = HDstrtod(tok[6], NULL);
            if (size == 0) {

                error_msg(progname, "Could not convert size to double\n");
                leave( EXIT_FAILURE);

            } /* end if */

            /* convert eoa from character character string */
            eoa = HDstrtod(tok[4], NULL);
            if (eoa == 0) {
    
                error_msg(progname, "Could not convert eoa to integer\n");
                leave( EXIT_FAILURE);

            } /* end if */

            /* if ( verbose ) printf("  size: %d\n", size); */

            /* transform body out of hexadecimal character string */
            body = HDmalloc(size);
            if (body == NULL) {

                error_msg(progname, "Could not allocate space for body\n");
                leave( EXIT_FAILURE);

            } /* end if */
            
            p = &(tok[10])[0];
            
            for (i = 0; i < size; i++) {
            
                body[i] = HDstrtoul(p, NULL, 16);
                p = &p[3];

            } /* end for */

            body[i] = 0;

            /* if ( verbose ) printf("  body: %s\n", body); */

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
            
                compare_buf = HDmalloc(size);
    
                if (compare_buf == NULL) {
                    error_msg(progname, "Could not allocate space\n");
                    leave( EXIT_FAILURE);
                } /* end if */
                
                pread(hdf5_fd, compare_buf, size, address);

                /* do a quick string compare on two items */
                if (HDstrcmp(body, compare_buf) != 0) {
                    error_msg(progname, "Entry incorrectly written into HDF5 file. Exiting.\n");
                    printf("Address %llx:\n", address);
                    printf(" -- from journal:   %llx\n", body);
                    printf(" -- from HDF5 file: %llx\n", compare_buf);
                    leave( EXIT_FAILURE );
                } /* end if */

                /* compare each individual value of entry */
                for (i=0; i<size; i++) {
                    if (body[i] != compare_buf[i]) {
                        error_msg(progname, "Entry incorrectly written into HDF5 file. Exiting.\n");
                        printf("Address %d\n", address + i);
                        printf(" -- from journal:   %d\n", body[i]);
                        printf(" -- from HDF5 file: %d\n", compare_buf[i]);
                        leave( EXIT_FAILURE );
                    }
                }

                if ( verbose ) printf(" .... SUCCESS!\n");
                free(compare_buf);

            }

            free(body);

        } /* end if */

    } /* end while */

    fclose(journal_fp);
    close(hdf5_fd);
    free(last_trans_msg);
    free(readback);
    free(journal_name);

    /************************
    * Mark HDF5 File as Recoverd *
    ************************/

    if ( verbose ) printf("Marking HDF5 File as recovered.\n");
    if ( verbose ) printf("==============================================\n\n");

    /* set up appropriate fapl */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    
    if ( fapl == -1 ) {
    
        error_msg(progname, "Could not create FAPL.\n");
        leave( EXIT_FAILURE );
    
    } /* end if */
    
    config.version = 1; /* should be H5C2__CURR_AUTO_SIZE_CTL_VER */
    
    /* get H5AC_cache_config_t configuration from fapl */
    if ( H5Pget_mdc_config(fapl, &config) == -1) {
    
        error_msg(progname, "Could not get mdc config from FAPL.\n");
        leave( EXIT_FAILURE );
    
    }
            
    /* make sure journal recovered field is set to TRUE in mdc_config */
    config.journal_recovered = TRUE;
    
    /* set H5AC_cache_config_t configuration with file recovered */
    if ( H5Pset_mdc_config(fapl, &config) == -1) {
    
        error_msg(progname, "Could not set mdc config on FAPL.\n");
        leave( EXIT_FAILURE );
    
    } /* end if */

    /* open HDF5 file with provided fapl */
    fid = H5Fopen(file_name, H5F_ACC_RDWR, fapl);    
       
    if ( fid == -1 ) {
    
        error_msg(progname, "Could not open recovered HDF5 file.\n");
        leave( EXIT_FAILURE );
    
    } /* end if */

    /* obtain H5F_t pointer */
    if ((f = H5I_object(fid)) == -1) {

        error_msg(progname, "Could not obtain H5F_t pointer from file id");
        leave( EXIT_FAILURE );

    } /* end if */
   
    /* set the correct value of the eoa */
    if (H5FDset_eoa(f->shared->lf, H5FD_MEM_DEFAULT, eoa) == -1) {

        error_msg(progname, "Driver set eoa request failed");
        leave( EXIT_FAILURE );

    } /* end if */

    /* close HDF5 file */
    if ( H5Fclose(fid) == -1 ) {
    
        error_msg(progname, "Could not close recovered HDF5 file.\n");
        leave( EXIT_FAILURE );
    
    } /* end if */
    
    printf("HDF5 file successfuly recovered.\n");

    free(file_name);

    return 1;
    
} /* end main */



