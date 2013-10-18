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

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5jam"

#define TRUE 1
#define FALSE 0

hsize_t write_pad (int, hsize_t);
hsize_t compute_user_block_size (hsize_t);
hsize_t copy_some_to_file (int, int, hsize_t, hsize_t, ssize_t);
void parse_command_line (int, const char *[]);

int do_clobber = FALSE;
char *output_file = NULL;
char *input_file = NULL;
char *ub_file = NULL;

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char *s_opts = "hi:u:o:c:V";  /* add more later ? */
static struct long_options l_opts[] = {
  {"help", no_arg, 'h'},
  {"hel", no_arg, 'h'},
  {"i", require_arg, 'i'},  /* input file */
  {"u", require_arg, 'u'},  /* user block file */
  {"o", require_arg, 'o'},  /* output file */
  {"clobber", no_arg, 'c'},  /* clobber existing UB */
  {"clobbe", no_arg, 'c'},
  {"clobb", no_arg, 'c'},
  {"clob", no_arg, 'c'},
  {"clo", no_arg, 'c'},
  {"cl", no_arg, 'c'},
  {NULL, 0, '\0'}
};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *prog)
{
    HDfflush (stdout);
    HDfprintf (stdout,
    "usage: %s -i <in_file.h5> -u <in_user_file> [-o <out_file.h5>] [--clobber]\n", prog);
    HDfprintf (stdout, "\n");
    HDfprintf (stdout,
    "Adds user block to front of an HDF5 file and creates a new concatenated file.\n");
    HDfprintf (stdout, "\n");
    HDfprintf (stdout, 
    "OPTIONS\n");
    HDfprintf (stdout, 
    "  -i in_file.h5    Specifies the input HDF5 file.\n");
    HDfprintf (stdout, 
    "  -u in_user_file  Specifies the file to be inserted into the user block.\n");
    HDfprintf (stdout, 
    "                   Can be any file format except an HDF5 format.\n");
    HDfprintf (stdout,
    "  -o out_file.h5   Specifies the output HDF5 file.\n");
    HDfprintf (stdout,
    "                   If not specified, the user block will be concatenated in\n");
    HDfprintf (stdout,
    "                   place to the input HDF5 file.\n");
    HDfprintf (stdout,
    "  --clobber        Wipes out any existing user block before concatenating\n");
    HDfprintf (stdout,
    "                   the given user block.\n");
    HDfprintf (stdout,
    "                   The size of the new user block will be the larger of;\n");
    HDfprintf (stdout,
    "                    - the size of existing user block in the input HDF5 file\n");
    HDfprintf (stdout,
    "                    - the size of user block required by new input user file\n");
    HDfprintf (stdout,
    "                   (size = 512 x 2N,  N is positive integer.)\n");
    HDfprintf (stdout, "\n");
    HDfprintf (stdout,
    "  -h               Prints a usage message and exits.\n");
    HDfprintf (stdout,
    "  -V               Prints the HDF5 library version and exits.\n");
    HDfprintf (stdout, "\n");
    HDfprintf (stdout,
    "Exit Status:\n");
    HDfprintf (stdout,
    "   0   Succeeded.\n");
    HDfprintf (stdout,
    "   >0  An error occurred.\n");
}


/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown and call exit()
 *
 * Return:      Does not return
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    if (ub_file)
        HDfree (ub_file);
    if (input_file)
        HDfree (input_file);
    if (output_file)
        HDfree (output_file);

    h5tools_close();

    HDexit(ret);
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
parse_command_line (int argc, const char *argv[])
{
  int opt = FALSE;

  /* parse command line options */
  while ((opt = get_option (argc, argv, s_opts, l_opts)) != EOF)
    {
      switch ((char) opt)
      {
      case 'o':
          output_file = HDstrdup (opt_arg);
          break;
      case 'i':
          input_file = HDstrdup (opt_arg);
          break;
      case 'u':
          ub_file = HDstrdup (opt_arg);
          break;
      case 'c':
          do_clobber = TRUE;
          break;
      case 'h':
          usage (h5tools_getprogname());
          leave (EXIT_SUCCESS);
      case 'V':
          print_version (h5tools_getprogname());
          leave (EXIT_SUCCESS);
      case '?':
      default:
          usage (h5tools_getprogname());
          leave (EXIT_FAILURE);
      }
    }
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 user block jammer
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, const char *argv[])
{
    int         ufid = -1;
    int         h5fid = -1;
    int         ofid = -1;
    void       *edata;
    H5E_auto2_t func;
    hid_t       ifile = -1;
    hid_t       plist = -1;
    herr_t      status;
    htri_t      testval;
    hsize_t     usize;
    hsize_t     h5fsize;
    hsize_t     startub;
    hsize_t     where;
    hsize_t     newubsize;
    off_t       fsize;
    h5_stat_t   sbuf;
    h5_stat_t   sbuf2;
    int         res;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    parse_command_line (argc, argv);

    if (ub_file == NULL) {
        /* no user block */
        error_msg("missing arguemnt for -u <user_file>.\n");
        help_ref_msg(stderr);
        leave (EXIT_FAILURE);
    }

    testval = H5Fis_hdf5 (ub_file);

    if (testval > 0) {
        error_msg("-u <user_file> cannot be HDF5 file, but it appears to be an HDF5 file.\n");
        help_ref_msg(stderr);
        leave (EXIT_FAILURE);
    }

    if (input_file == NULL) {
        error_msg("missing arguemnt for -i <HDF5 file>.\n");
        help_ref_msg(stderr);
        leave (EXIT_FAILURE);
    }

    testval = H5Fis_hdf5 (input_file);

    if (testval <= 0) {
        error_msg("Input HDF5 file \"%s\" is not HDF5 format.\n", input_file);
        help_ref_msg(stderr);
        leave (EXIT_FAILURE);
    }

    ifile = H5Fopen (input_file, H5F_ACC_RDONLY, H5P_DEFAULT);

    if (ifile < 0) {
        error_msg("Can't open input HDF5 file \"%s\"\n", input_file);
        leave (EXIT_FAILURE);
    }

    plist = H5Fget_create_plist (ifile);
    if (plist < 0) {
        error_msg("Can't get file creation plist for file \"%s\"\n", input_file);
        H5Fclose(ifile);
        leave (EXIT_FAILURE);
    }

    status = H5Pget_userblock (plist, &usize);
    if (status < 0) {
        error_msg("Can't get user block for file \"%s\"\n", input_file);
        H5Pclose(plist);
        H5Fclose(ifile);
        leave (EXIT_FAILURE);
    }

    H5Pclose(plist);
    H5Fclose(ifile);

    ufid = HDopen(ub_file, O_RDONLY, 0);
    if(ufid < 0) {
        error_msg("unable to open user block file \"%s\"\n", ub_file);
        leave (EXIT_FAILURE);
    }

    res = HDfstat(ufid, &sbuf);
    if(res < 0) {
        error_msg("Can't stat file \"%s\"\n", ub_file);
        HDclose (ufid);
        leave (EXIT_FAILURE);
    }

    fsize = (off_t)sbuf.st_size;

    h5fid = HDopen(input_file, O_RDONLY, 0);
    if(h5fid < 0) {
        error_msg("unable to open HDF5 file for read \"%s\"\n", input_file);
        HDclose (ufid);
        leave (EXIT_FAILURE);
    }

    res = HDfstat(h5fid, &sbuf2);
    if(res < 0) {
        error_msg("Can't stat file \"%s\"\n", input_file);
        HDclose (h5fid);
        HDclose (ufid);
        leave (EXIT_FAILURE);
    }

    h5fsize = (hsize_t)sbuf2.st_size;

    if (output_file == NULL) {
        ofid = HDopen (input_file, O_WRONLY, 0);

        if (ofid < 0) {
            error_msg("unable to open output file \"%s\"\n", output_file);
            HDclose (h5fid);
            HDclose (ufid);
            leave (EXIT_FAILURE);
        }
    }
    else {
        ofid = HDopen (output_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);

        if (ofid < 0) {
            error_msg("unable to create output file \"%s\"\n", output_file);
            HDclose (h5fid);
            HDclose (ufid);
            leave (EXIT_FAILURE);
        }
    }

    newubsize = compute_user_block_size ((hsize_t) fsize);

    startub = usize;

    if (usize > 0) {
        if (do_clobber == TRUE) {
            /* where is max of the current size or the new UB */
            if (usize > newubsize) {
                newubsize = usize;
            }
            startub = 0;    /*blast the old */
        }
        else {
            /* add new ub to current ublock, pad to new offset */
            newubsize += usize;
            newubsize = compute_user_block_size ((hsize_t) newubsize);
        }
    }

    /* copy the HDF5 from starting at usize to starting at newubsize:
     *  makes room at 'from' for new ub */
    /* if no current ub, usize is 0 */
    copy_some_to_file (h5fid, ofid, usize, newubsize, (ssize_t) (h5fsize - usize));

    /* copy the old ub to the beginning of the new file */
    if (!do_clobber) {
        where = copy_some_to_file (h5fid, ofid, (hsize_t) 0, (hsize_t) 0, (ssize_t) usize);
    }

    /* copy the new ub to the end of the ub */
    where = copy_some_to_file (ufid, ofid, (hsize_t) 0, startub, (ssize_t) - 1);

    /* pad the ub */
    where = write_pad (ofid, where);

    if(ub_file)
        HDfree (ub_file);
    if(input_file)
        HDfree (input_file);
    if(output_file)
        HDfree (output_file);
    
    if(ufid >= 0)
        HDclose (ufid);
    if(h5fid >= 0)
        HDclose (h5fid);
    if(ofid >= 0)
        HDclose (ofid);

    return h5tools_getstatus();
}

/*-------------------------------------------------------------------------
 * Function:    copy_some_to_file
 *
 * Purpose:     Copy part of the input file to output.
 *      infid: fd of file to read
 *      outfid: fd of file to write
 *      startin: offset of where to read from infid
 *      startout: offset of where to write to outfid
 *      limit: bytes to read/write
 *
 *    If limit is < 0, the entire input file is copied.
 *
 *    Note: this routine can be used to copy within
 *    the same file, i.e., infid and outfid can be the
 *    same file.
 *
 * Return:      Success:    last byte written in the output.
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
copy_some_to_file (int infid, int outfid, hsize_t startin, hsize_t startout,
       ssize_t limit)
{
  char buf[1024];
  h5_stat_t sbuf;
  int res;
  ssize_t tot = 0;
  ssize_t howmuch = 0;
  ssize_t nchars = -1;
/* used in assertion check
  ssize_t ncw = -1;
*/
  ssize_t to;
  ssize_t from;
  ssize_t toend;
  ssize_t fromend;

  if(startin > startout) {
      /* this case is prohibited */
      error_msg("copy_some_to_file: panic: startin > startout?\n");
      exit (EXIT_FAILURE);
    }

  if(limit < 0) {
      res = HDfstat(infid, &sbuf);
      if(res < 0) {
    error_msg("Can't stat file \n");
    exit (EXIT_FAILURE);
  }

      howmuch = (ssize_t)sbuf.st_size;
    }
  else
      howmuch = limit;

  if(howmuch == 0)
      return 0;

  /* assert (howmuch > 0) */

  toend = (ssize_t) startout + howmuch;
  fromend = (ssize_t) startin + howmuch;

  if (howmuch > 512)
    {
      to = toend - 512;
      from = fromend - 512;
    }
  else
    {
      to = toend - howmuch;
      from = fromend - howmuch;
    }

  while (howmuch > 0)
    {
      HDlseek (outfid, (off_t) to, SEEK_SET);
      HDlseek (infid, (off_t) from, SEEK_SET);

      if (howmuch > 512)
  {
    nchars = HDread (infid, buf, (unsigned) 512);
  }
      else
  {
    nchars = HDread (infid, buf, (unsigned)howmuch);
  }

      if (nchars <= 0)
  {
    printf ("huh? \n");
    exit (EXIT_FAILURE);
  }
      /*ncw = */ HDwrite (outfid, buf, (unsigned) nchars);

      /* assert (ncw == nchars) */

      tot += nchars;
      howmuch -= nchars;
      if (howmuch > 512)
  {
    to -= nchars;
    from -= nchars;
  }
      else
  {
    to -= howmuch;
    from -= howmuch;
  }
    }

  /* assert howmuch == 0 */
  /* assert tot == limit */

  return ((hsize_t) tot + (hsize_t) startout);
}


/*-------------------------------------------------------------------------
 * Function:    compute_user_block_size
 *
 * Purpose:     Find the offset of the HDF5 header after the user block:
 *                 align at 0, 512, 1024, etc.
 *      ublock_size: the size of the user block (bytes).
 *
 * Return:      Success:    the location of the header == the size of the
 *        padded user block.
 *              Failure:    none
 *
 * Return:      Success:    last byte written in the output.
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
compute_user_block_size (hsize_t ublock_size)
{
  hsize_t where = 512;

  if (ublock_size == 0)
    return 0;

  while (where < ublock_size)
    {
      where *= 2;
    }

  return (where);
}

/*
 *  Write zeroes to fill the file from 'where' to 512, 1024, etc. bytes.
 *
 *  Returns the size of the padded file.
 */
hsize_t
write_pad(int ofile, hsize_t where)
{
    unsigned int i;
    char buf[1];
    hsize_t psize;

    buf[0] = '\0';

    HDlseek(ofile, (off_t) where, SEEK_SET);

    psize = compute_user_block_size (where);
    psize -= where;

    for(i = 0; i < psize; i++)
        HDwrite (ofile, buf, 1);

    return(where + psize);  /* the new size of the file. */
}

