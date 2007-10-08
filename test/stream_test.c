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

/*
 * Copyright © 2000 The author.
 * The author prefers this code not be used for military purposes.
 *
 *
 * Author:  Thomas Radke <tradke@aei-potsdam.mpg.de>
 *          Tuesday, September 12, 2000
 *
 * Modifications:
 *          Thomas Radke, Thursday, October 26, 2000
 *          Made it compiling under Windows.
 *
 */

/*
 *  This program tests the functionality of the Stream Virtual File Driver.
 *    1. It spawns two new processes, a sender and a receiver.
 *    2. The sender opens an HDF5 file for writing using the Stream driver.
 *       It will use a reserved port which should fail to be bound.
 *       Then it will try a couple of successive ports until bind succeeds.
 *       This final "hostname:port" information is written into a temporary
 *       file as a single line of text.
 *       The sender then writes a sample dataset to the HDF5 file.
 *       On closing the file the Stream VFD would send the file
 *       contents to any connected client.
 *    3. The receiver serves as a client attempting to open an
 *       HDF5 file for reading. On opening the file the Stream VFD
 *       would establish a socket connection to the sender process,
 *       identified by its hostname and a port number (which is obtained
 *       from the temporary text file the sender should have created),
 *       and read the file contents via this socket.
 *       Aftwerwards the dataset is read from the file into memory
 *       and verified.
 *    4. The main program waits for termination of its two child
 *       processes and returns their exit code.
 */

#include <stdio.h>
#include "hdf5.h"

#ifndef H5_HAVE_STREAM

int main (void)
{
  printf ("Test skipped because Stream Virtual File Driver not available\n");
  return (0);
}

#elif ! defined (H5_HAVE_FORK) || ! defined (H5_HAVE_WAITPID)

int main (void)
{
  printf ("Test skipped because this architecture doesn't provide "
          "fork(2) and waitpid(2)\n");
  return (0);
}

#else

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>


#define SLEEPTIME     10                  /* sleeping time in seconds    */
#define RANK          2                   /* sample dataset rank         */
#define DIMS          50                  /* sample dataset dimensions   */
#define DATASETNAME   "IntArray"          /* sample dataset name         */
#define HOSTNAME      "localhost"         /* hostname of this machine    */
#define PORT          "5678"              /* default port to use         */
#define MAXHUNT       500                 /* max number of ports to hunt */
#define HDF5_FILENAME HOSTNAME ":" PORT   /* name of the streamed file   */
#define TEMPFILENAME  "stream_test.tmp"   /* temporary filename          */


static int sender (void)
{
  int     i;
  hsize_t dims[RANK];
  int     *data;
  herr_t  status;
  hid_t   fapl, file;
  hid_t   dataspace, dataset;
  H5FD_stream_fapl_t stream_fapl;
  FILE    *tempfile;


  /*
   * Create access property list and set it to use the Stream driver.
   */
  fapl = H5Pcreate (H5P_FILE_ACCESS);
  if (fapl < 0)
  {
    fprintf (stderr, "sender: couldn't create file access property list\n");
    return (-1);
  }

  /*
   * Setup file access property list and select Stream VFD.
   *
   *   - block increment for realloc() should be chosen by the driver
   *   - no external socket is provided (should be created internally)
   *   - do I/O on this processor on this socket
   *   - only one client is allowed to connect at a time
   *   - no READ broadcast function is provided (since we only send data)
   *   - if bind to default port (given in the filename argument) fails
   *     do port hunting on the following MAXHUNT ports
   */
  stream_fapl.increment = 0;
  stream_fapl.socket = H5FD_STREAM_INVALID_SOCKET;
  stream_fapl.do_socket_io = 1;
  stream_fapl.backlog = 1;
  stream_fapl.broadcast_fn  = NULL;
  stream_fapl.broadcast_arg = NULL;
  stream_fapl.maxhunt = MAXHUNT;

  status = H5Pset_fapl_stream (fapl, &stream_fapl);
  if (status < 0)
  {
    fprintf (stderr, "sender: couldn't set file access property list "
                     "for Stream VFD\n");
    H5Pclose (fapl);
    return (-2);
  }

  /*
   * Create the data space for fixed size dataset.
   */
  for (i = 0; i < RANK; i++)
  {
    dims[i] = DIMS;
  }
  dataspace = H5Screate_simple (RANK, dims, NULL);
  if (dataspace < 0)
  {
    fprintf (stderr, "sender: couldn't create dataspace\n");
    H5Pclose (fapl);
    return (-3);
  }

  /*
   * Data buffer initialization.
   */
  i = (int) H5Sget_simple_extent_npoints (dataspace);
  data = (int *) malloc (i * sizeof (int));
  if (data == NULL)
  {
    fprintf (stderr, "sender: cannot allocate buffer for dataset with "
                     "%d integers\n", i);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-4);
  }
  while (--i >= 0)
  {
    data[i] = i;
  }

  /*
   * Create a new file using H5F_ACC_TRUNC access,
   * default file creation properties, and STREAM file
   * access properties.
   */
  printf ("  sender: opening file on host '%s' port %s for writing...\n",
          HOSTNAME, PORT);
  file = H5Fcreate (HDF5_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  if (file < 0)
  {
    fprintf (stderr, "sender: couldn't create file on '%s' using port %s and "
            "following %d\n", HOSTNAME, PORT, MAXHUNT);
    free (data);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-5);
  }

  /*
   * Get the file access property list to find out what port is actually used.
   */
  status = H5Pget_fapl_stream (fapl, &stream_fapl);
  if (status < 0)
  {
    fprintf (stderr, "sender: couldn't get file access property list "
                     "for Stream VFD\n");
    free (data);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-6);
  }
  printf ("  sender: using port %d...\n", (int) stream_fapl.port);

  /*
   * Write the "hostname:port" information to a temporary file
   * which can be read by the receiver process.
   */
  tempfile = fopen (TEMPFILENAME, "w");
  if (tempfile == NULL)
  {
    fprintf (stderr, "sender: couldn't open temporary file to write "
                     "\"hostname:port\" information\n");
    free (data);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-7);
  }
  fprintf (tempfile, "%s:%d", HOSTNAME, (int) stream_fapl.port);
  fclose (tempfile);

  /*
   * Create a new dataset within the file using defined dataspace and
   * default dataset creation properties.
   */
  dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, dataspace,
                       H5P_DEFAULT);
  if (dataset < 0)
  {
    fprintf (stderr, "sender: couldn't create dataset '%s'\n", DATASETNAME);
    free (data);
    H5Fclose (file);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-8);
  }

  /*
   * Write the data to the dataset using default transfer properties.
   */
  printf ("  sender: writing dataset '%s' of type INTEGER to file '%s:%d'...\n",
          DATASETNAME, HOSTNAME, (int) stream_fapl.port);
  status = H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                     data);
  if (status < 0)
  {
    free (data);
    H5Fclose (file);
    H5Dclose (dataset);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    fprintf (stderr, "sender: couldn't write dataset\n");
    return (-9);
  }

  /*
   * Now give the receiver some time to connect before closing the file
   * and releasing resources.
   */
  printf ("  sender: sleeping for %d seconds...\n", SLEEPTIME);
  sleep (SLEEPTIME);
  printf ("  sender: closing file '%s:%d'\n", HOSTNAME, (int) stream_fapl.port);
  H5Sclose (dataspace);
  H5Dclose (dataset);
  H5Fclose (file);
  H5Pclose (fapl);
  free (data);

  return (0);
}


static int receiver (void)
{
  int i;                      /* looper */
  hid_t fapl;                 /* file access property list */
  hid_t file;                 /* file handle */
  hid_t dataset;              /* dataset handle */
  hid_t datatype;             /* datatype handle */
  hid_t dataspace;            /* dataspace handle */
  hsize_t nelems;             /* total number of elements in the dataset */
  hsize_t *dims;              /* dataset dimensions */
  int rank;                   /* dataset rank */
  int *data;                  /* read buffer */
  int nerrors;                /* total number of errors during verify */
  int status;                 /* return code of HDF5 routines */
  char filename[50];          /* filename of the streamed HDF5 file */
  FILE    *tempfile;          /* descriptor for temporary file */


  /*
   * Create access property list and set it to use the Stream driver.
   */
  fapl = H5Pcreate (H5P_FILE_ACCESS);
  if (fapl < 0)
  {
    fprintf (stderr, "receiver: couldn't create file access property list\n");
    return (-1);
  }

  status = H5Pset_fapl_stream (fapl, NULL);
  if (status < 0)
  {
    fprintf (stderr, "receiver: couldn't set file access property list "
                     "for Stream VFD\n");
    H5Pclose (fapl);
    return (-2);
  }

  /*
   * Now give the sender some time to open the file and accepting connections.
   */
  printf ("  receiver: sleeping for %d seconds...\n", SLEEPTIME / 2);
  sleep (SLEEPTIME / 2);

  /*
   * Read the "hostname:port" information from the temporary file
   * the sender should have created.
   */
  tempfile = fopen (TEMPFILENAME, "r");
  if(tempfile == NULL) {
    fprintf(stderr, "receiver: couldn't open temporary file to read "
                     "\"hostname:port\" information\n");
    H5Pclose(fapl);
    return(-3);
  }
  fgets(filename, sizeof (filename) - 1, tempfile);
  fclose(tempfile);
  unlink(TEMPFILENAME);

  /*
   * Open the streamed HDF5 file for reading.
   */
  printf("  receiver: opening file '%s' for reading...\n", filename);
  file = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
  H5Pclose(fapl);
  if(file < 0) {
    fprintf (stderr, "receiver: couldn't open file from '%s'\n", filename);
    return (-4);
  }

  /*
   * Open the file and the dataset.
   */
  printf ("  receiver: reading dataset '%s'...\n", DATASETNAME);
  dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT);
  if(dataset < 0) {
    fprintf (stderr, "receiver: couldn't open dataset '%s'\n", DATASETNAME);
    return (-5);
  }

  /*
   * Get dataset class, order, and size information
   */
  datatype = H5Dget_type(dataset);
  if(H5Tget_class(datatype) == H5T_INTEGER)
    printf("  receiver: dataset is of type INTEGER\n");
  printf("  receiver: datatype size is %d bytes\n",
          (int) H5Tget_size (datatype));
  printf("  receiver: byte ordering is %s endian\n",
          H5Tget_order (datatype) == H5T_ORDER_LE ? "little" : "big");
  H5Tclose(datatype);

  /*
   * Get dataset dimensions
   */
  dataspace = H5Dget_space(dataset);
  rank      = H5Sget_simple_extent_ndims(dataspace);
  dims      = (hsize_t *)malloc(rank * sizeof (hsize_t));
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  H5Sclose(dataspace);

  printf("  receiver: rank %d, dimensions %u", rank, (unsigned int)dims[0]);
  nelems = dims[0];
  for (i = 1; i < rank; i++)
  {
    printf (" x %u", (unsigned int) dims[i]);
    nelems *= dims[i];
  }
  printf ("\n  receiver: total number of elements: %d\n", (int) nelems);
  free (dims);

  /*
   * Read dataset from file into memory.
   */
  data = (int *) malloc ((size_t) nelems * sizeof (int));
  status = H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                    data);
  H5Dclose (dataset);

  /*
   * Close the file.
   */
  printf ("  receiver: closing file '%s'...\n", filename);
  H5Fclose (file);

  /*
   * Verify the dataset contents
   */
  printf ("  receiver: verifying contents of dataset '%s'...\n", DATASETNAME);
  for (i = nerrors = 0; i < (int) nelems; i++)
  {
    if (data[i] != i)
    {
      fprintf (stderr, "receiver: data error at offset %d: "
                       "expected %d got %d\n", i, i, data[i]);
      nerrors++;
    }
  }
  printf ("  receiver: dataset verified, %d errors found\n", nerrors);

  free (data);

  return (-nerrors);
}


int main (void)
{
  int main_status, sender_status, receiver_status;
  pid_t sender_pid, receiver_pid;


  sender_pid = receiver_pid = 0;

  /* main's return code for success */
  main_status = 0;

  /* spawn off the sender and the receiver process */
  printf ("main: starting sender process...\n");
  sender_pid = fork ();
  if (sender_pid == 0)
  {
    return (sender ());
  }
  else if (sender_pid < 0)
  {
    perror ("Failed to spawn sender");
    main_status = -1;
  }
  else
  {
    printf ("main: starting receiver process...\n");
    receiver_pid = fork ();
    if (receiver_pid == 0)
    {
      return (receiver ());
    }
    else if (sender_pid < 0)
    {
      perror ("Failed to spawn receiver");
      main_status = -1;
    }
  }

  /* wait for the termination of sender and receiver and check their status */
  printf ("main: waiting for termination of sender and receiver process...\n");
  if (sender_pid > 0 &&
      waitpid (sender_pid, &sender_status, 0) != sender_pid)
  {
    perror ("Failed to wait for termination of sender");
    main_status = -1;
  }
  else
  {
    main_status |= sender_status;
  }
  if (receiver_pid > 0 &&
      waitpid (receiver_pid, &receiver_status, 0) != receiver_pid)
  {
    perror ("Failed to wait for termination of receiver");
    main_status = -1;
  }
  else
  {
    main_status |= receiver_status;
  }

  printf (main_status == 0 ?
          "Stream Virtual File Driver test passed.\n" :
          "*** Stream Virtual File Driver TEST FAILED ***\n");

  return (main_status);
}

#endif /* H5_HAVE_STREAM */
