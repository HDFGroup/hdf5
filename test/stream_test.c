/*
 * Copyright © 2000 The author.
 * The author prefers this code not be used for military purposes.
 *
 *
 * Author:  Thomas Radke <tradke@aei-potsdam.mpg.de>
 *          Tuesday, September 12, 2000
 *
 * Version: $Id$
 *
 * Modifications:
 *          Thomas Radke, Thursday, October 26, 2000
 *          Made it compiling under Windows.
 *
 */

/*
 *  This program tests the functionality of the Stream Virtual File Driver.
 *    1. It spawns two new processes, a sender and a receiver.
 *    2. The sender opens an HDF5 file for writing and writes
 *       a sample dataset to it.
 *       On closing the file the Stream VFD would send the file
 *       contents to any connected client.
 *    3. The receiver serves as a client attempting to open an
 *       HDF5 file for reading. On opening the file the Stream VFD
 *       would establish a socket connection to the sender process,
 *       identified by its hostname (which is localhost in this example)
 *       and a port number, and read the file contents via this socket.
 *       Aftwerwards the dataset is read from the file into memory
 *       and verified.
 *    4. The main program waits for termination of its two child
 *       processes and returns their exit code.
 */

#include <stdio.h>
#include <hdf5.h>

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


#define DELAY         10                /* sleeping time in seconds  */
#define RANK          2                 /* sample dataset rank       */
#define DIMS          50                /* sample dataset dimensions */
#define DATASETNAME   "IntArray"        /* sample dataset name       */
#define FILENAME      "localhost:5678"  /* filename argument         */


static int sender (void)
{
  int     i;
  hsize_t dims[RANK];
  int     *data;
  herr_t  status;
  hid_t   fapl, file;
  hid_t   dataspace, dataset;


  /*
   * Create access property list and set it to use the Stream driver.
   */
  fapl = H5Pcreate (H5P_FILE_ACCESS);
  if (fapl < 0)
  {
    fprintf (stderr, "sender: couldn't create file access property list\n");
    return (-1);
  }

  status = H5Pset_fapl_stream (fapl, NULL);
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
  printf ("  sender: opening file '%s' for writing...\n", FILENAME);
  file = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  if (file < 0)
  {
    fprintf (stderr, "sender: couldn't create file for '%s'\n", FILENAME);
    free (data);
    H5Sclose (dataspace);
    H5Pclose (fapl);
    return (-5);
  }

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
    return (-6);
  }

  /*
   * Write the data to the dataset using default transfer properties.
   */
  printf ("  sender: writing dataset '%s' of type INTEGER to file '%s'...\n",
          DATASETNAME, FILENAME);
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
    return (-7);
  }

  /*
   * Now give the receiver some time to connect before closing the file
   * and releasing resources.
   */
  printf ("  sender: sleeping for %d seconds...\n", DELAY);
  sleep (DELAY);
  printf ("  sender: closing file '%s'\n", FILENAME);
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
    return (-2);
  }

  /*
   * Now give the sender some time to open the file and accepting connections.
   */
  printf ("  receiver: sleeping for %d seconds...\n", DELAY / 2);
  sleep (DELAY / 2);
  printf ("  receiver: opening file '%s' for reading...\n", FILENAME);
  file = H5Fopen (FILENAME, H5F_ACC_RDONLY, fapl);
  H5Pclose (fapl);
  if (file < 0)
  {
    fprintf (stderr, "receiver: couldn't open file from '%s'\n", FILENAME);
    return (-3);
  }

  /*
   * Open the file and the dataset.
   */
  printf ("  receiver: reading dataset '%s'...\n", DATASETNAME);
  dataset = H5Dopen (file, DATASETNAME);
  if (dataset < 0)
  {
    fprintf (stderr, "receiver: couldn't open dataset '%s'\n", DATASETNAME);
    return (-4);
  }

  /*
   * Get dataset class, order, and size information
   */
  datatype = H5Dget_type (dataset);
  if (H5Tget_class (datatype) == H5T_INTEGER)
  {
    printf ("  receiver: dataset is of type INTEGER\n");
  }
  printf ("  receiver: datatype size is %d bytes\n",
          (int) H5Tget_size (datatype));
  printf ("  receiver: byte ordering is %s endian\n",
          H5Tget_order (datatype) == H5T_ORDER_LE ? "little" : "big");
  H5Tclose(datatype);

  /*
   * Get dataset dimensions
   */
  dataspace = H5Dget_space (dataset);
  rank      = H5Sget_simple_extent_ndims (dataspace);
  dims      = (hsize_t *) malloc (rank * sizeof (hsize_t));
  H5Sget_simple_extent_dims (dataspace, dims, NULL);
  H5Sclose (dataspace);

  printf ("  receiver: rank %d, dimensions %u", rank, (unsigned int) dims[0]);
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
  data = (int *) malloc ((size_t)(nelems * sizeof (int)));
  status = H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                    data);
  H5Dclose (dataset);

  /*
   * Close the file.
   */
  printf ("  receiver: closing file '%s'...\n", FILENAME);
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
