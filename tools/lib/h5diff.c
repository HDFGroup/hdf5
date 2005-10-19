/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include "h5diff.h"
#include "H5private.h"
#include "ph5diff.h"

/*
 * Debug printf macros. The prefix allows output filtering by test scripts.
 */
#ifdef debug
#define h5diffdebug(x)	fprintf(stderr, "h5diff debug: " x)
#define h5diffdebug2(x1, x2)	fprintf(stderr, "h5diff debug: " x1, x2)
#define h5diffdebug3(x1, x2, x3)	fprintf(stderr, "h5diff debug: " x1, x2, x3)
#define h5diffdebug4(x1, x2, x3, x4)	fprintf(stderr, "h5diff debug: " x1, x2, x3, x4)
#define h5diffdebug5(x1, x2, x3, x4, x5)	fprintf(stderr, "h5diff debug: " x1, x2, x3, x4, x5)
#else
#define h5diffdebug(x)
#define h5diffdebug2(x1, x2)
#define h5diffdebug3(x1, x2, x3)
#define h5diffdebug4(x1, x2, x3, x4)
#define h5diffdebug5(x1, x2, x3, x4, x5)
#endif


/*-------------------------------------------------------------------------
 * Function: print_objname
 *
 * Purpose: print object name only when:
 *  1) verbose mode
 *  2) when diff was found (normal mode)
 *-------------------------------------------------------------------------
 */
int
print_objname (diff_opt_t * options, hsize_t nfound)
{
  return ((options->m_verbose || nfound) && !options->m_quiet) ? 1 : 0;
}

#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function: phdiff_dismiss_workers
 *
 * Purpose: tell all workers to end.
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 *
 * Date: Feb 6, 2005
 *
 *-------------------------------------------------------------------------
 */
void phdiff_dismiss_workers(void)
{
    int i;
    for(i=1; i<g_nTasks; i++)
	MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);
}


/*-------------------------------------------------------------------------
 * Function: print_manager_output
 *
 * Purpose: special function that prints any output accumulated by the
 *      manager task.
 *
 * Return: none
 *
 * Programmer: Leon Arber
 *
 * Date: Feb 7, 2005
 *
 *-------------------------------------------------------------------------
 */
void print_manager_output(void)
{
    /* If there was something we buffered, let's print it now */
    if( (outBuffOffset>0) && g_Parallel)
    {
	printf("%s", outBuff);

	if(overflow_file)
	{
	    int     tmp;

	    rewind(overflow_file);
	    while((tmp = getc(overflow_file)) >= 0)
		putchar(tmp);

	    fclose(overflow_file);
	    overflow_file = NULL;
	}

	fflush(stdout);
	memset(outBuff, 0, OUTBUFF_SIZE);
	outBuffOffset = 0;
    }
    else if( (outBuffOffset>0) && !g_Parallel)
    {
	fprintf(stderr, "h5diff error: outBuffOffset>0, but we're not in parallel!\n");
    }
}

/*-------------------------------------------------------------------------
 * Function: print_incoming_data
 *
 * Purpose: special function that prints any output that has been sent to the manager
 *      and is currently sitting in the incoming message queue
 *
 * Return: none
 *
 * Programmer: Leon Arber
 *
 * Date: March 7, 2005
 *
 *-------------------------------------------------------------------------
 */

static void print_incoming_data(void)
{
    char data[PRINT_DATA_MAX_SIZE+1];
    int  incomingMessage;
    MPI_Status Status;

    do
    {
	MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &incomingMessage, &Status);
	if(incomingMessage)
	{
	    memset(data, 0, PRINT_DATA_MAX_SIZE+1);
	    MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

	    printf("%s", data);
	}
    } while(incomingMessage);
}
#endif

/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose: public function, can be called in an application program.
 *   return differences between 2 HDF5 files
 *
 * Return: Number of differences found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 22, 2003
 *
 * Modifications: Jan 2005 Leon Arber, larber@uiuc.edu
 *    Added support for parallel diffing
 *
 *-------------------------------------------------------------------------
 */

hsize_t
h5diff (const char *fname1,
 const char *fname2,
 const char *objname1, const char *objname2, diff_opt_t * options)
{
  int nobjects1, nobjects2;
  trav_info_t *info1 = NULL;
  trav_info_t *info2 = NULL;
  hid_t        file1_id=(-1), file2_id=(-1);
  char filenames[2][1024];
  hsize_t nfound = 0;
  int not_cmp = 0;

  memset(filenames, 0, 1024*2);


  if (options->m_quiet && (options->m_verbose || options->m_report))
    {
      printf
 ("Error: -q (quiet mode) cannot be added to verbose or report modes\n");
      options->err_stat = 1;
      return 0;
    }

/*-------------------------------------------------------------------------
 * open the files first; if they are not valid, no point in continuing
 *-------------------------------------------------------------------------
 */

  /* disable error reporting */
  H5E_BEGIN_TRY
  {
    /* Open the files */
    if ((file1_id = H5Fopen (fname1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
      {
 printf ("h5diff: <%s>: unable to open file\n", fname1);
 options->err_stat = 1;

#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     /* Let tasks know that they won't be needed */
     phdiff_dismiss_workers();
 }
#endif

 goto out;
      }
    if ((file2_id = H5Fopen (fname2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
    {
 printf ("h5diff: <%s>: unable to open file\n", fname2);
 options->err_stat = 1;

#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     /* Let tasks know that they won't be needed */
     phdiff_dismiss_workers();
 }
#endif

 goto out;
      }
    /* enable error reporting */
  }
  H5E_END_TRY;


/*-------------------------------------------------------------------------
 * get the number of objects in the files
 *-------------------------------------------------------------------------
 */
  nobjects1 = h5trav_getinfo (file1_id, NULL, 0);
  nobjects2 = h5trav_getinfo (file2_id, NULL, 0);

  if (nobjects1 < 0 || nobjects2 < 0)
    {
      printf ("Error: Could not get get file contents\n");
      options->err_stat = 1;
#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     /* Let tasks know that they won't be needed */
     phdiff_dismiss_workers();
 }
#endif
      goto out;
    }

 

/*-------------------------------------------------------------------------
 * get the list of objects in the files
 *-------------------------------------------------------------------------
 */

  info1 = (trav_info_t *) malloc (nobjects1 * sizeof (trav_info_t));
  info2 = (trav_info_t *) malloc (nobjects2 * sizeof (trav_info_t));
  if (info1 == NULL || info2 == NULL)
    {
      printf ("Error: Not enough memory for object list\n");
      options->err_stat = 1;
      if (info1)
 h5trav_freeinfo (info1, nobjects1);
      if (info2)
 h5trav_freeinfo (info2, nobjects1);
#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     /* Let tasks know that they won't be needed */
     phdiff_dismiss_workers();
 }
#endif
      goto out;
    }

  h5trav_getinfo (file1_id, info1, 0);
  h5trav_getinfo (file2_id, info2, 0);

/*-------------------------------------------------------------------------
 * object name was supplied
 *-------------------------------------------------------------------------
 */

  if (objname1)
    {

#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     /* Let tasks know that they won't be needed */
     phdiff_dismiss_workers();
 }
#endif
      assert (objname2);
      options->cmn_objs = 1; /* eliminate warning */
      nfound = diff_compare (file1_id, fname1, objname1, nobjects1, info1,
        file2_id, fname2, objname2, nobjects2, info2,
        options);
    }

/*-------------------------------------------------------------------------
 * compare all
 *-------------------------------------------------------------------------
 */

  else
  {

#ifdef H5_HAVE_PARALLEL
      if(g_Parallel)
      {
          int i;

   if(  (strlen(fname1) > 1024) || (strlen(fname2) > 1024))
   {
       fprintf(stderr, "The parallel diff only supports path names up to 1024 characters\n");
       MPI_Abort(MPI_COMM_WORLD, 0);
   }

   strcpy(filenames[0], fname1);
   strcpy(filenames[1], fname2);

   /* Alert the worker tasks that there's going to be work. */

   for(i=1; i<g_nTasks; i++)
       MPI_Send(filenames, 1024*2, MPI_CHAR, i, MPI_TAG_PARALLEL, MPI_COMM_WORLD);
      }
#endif
      nfound = diff_match (file1_id, nobjects1, info1,
       file2_id, nobjects2, info2, options);

  }


  h5trav_freeinfo (info1, nobjects1);
  h5trav_freeinfo (info2, nobjects2);

out:
  /* close */
  H5E_BEGIN_TRY
  {
      H5Fclose (file1_id);
      H5Fclose (file2_id);
  }
  H5E_END_TRY;

  return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose: Find common objects; the algorithm used for this search is the
 *  cosequential match algorithm and is described in
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Modifications: Jan 2005 Leon Arber, larber@uiuc.edu
 *    Added support for parallel diffing
 *
 *-------------------------------------------------------------------------
 */
hsize_t
diff_match (hid_t file1_id,
 int nobjects1,
 trav_info_t * info1,
 hid_t file2_id,
 int nobjects2, trav_info_t * info2, diff_opt_t * options)
{
    int more_names_exist = (nobjects1 > 0 && nobjects2 > 0) ? 1 : 0;
    trav_table_t *table = NULL;
    int cmp;
    int curr1 = 0;
    int curr2 = 0;
    unsigned infile[2];
    char c1, c2;
    hsize_t nfound = 0;
    int i;

    /*-------------------------------------------------------------------------
     * build the list
     *-------------------------------------------------------------------------
     */
    trav_table_init (&table);

    while (more_names_exist)
    {
 /* criteria is string compare */
 cmp = strcmp (info1[curr1].name, info2[curr2].name);
 if (cmp == 0)
 {
     infile[0] = 1;
     infile[1] = 1;
     trav_table_addflags (infile, info1[curr1].name, info1[curr1].type,
      table);

     curr1++;
     curr2++;
 }
 else if (cmp < 0)
 {
     infile[0] = 1;
     infile[1] = 0;
     trav_table_addflags (infile, info1[curr1].name, info1[curr1].type,
      table);
     curr1++;
 }
 else
 {
     infile[0] = 0;
     infile[1] = 1;
     trav_table_addflags (infile, info2[curr2].name, info2[curr2].type,
      table);
     curr2++;
 }

 more_names_exist = (curr1 < nobjects1 && curr2 < nobjects2) ? 1 : 0;


    }    /* end while */

    /* list1 did not end */
    if (curr1 < nobjects1)
    {
 while (curr1 < nobjects1)
 {
     infile[0] = 1;
     infile[1] = 0;
     trav_table_addflags (infile, info1[curr1].name, info1[curr1].type,
      table);
     curr1++;
 }
    }

    /* list2 did not end */
    if (curr2 < nobjects2)
    {
 while (curr2 < nobjects2)
 {
     infile[0] = 0;
     infile[1] = 1;
     trav_table_addflags (infile, info2[curr2].name, info2[curr2].type,
      table);
     curr2++;
 }
    }

    /*-------------------------------------------------------------------------
     * print the list
     *-------------------------------------------------------------------------
     */

    if (options->m_verbose)
    {
 printf ("\n");
 printf ("file1     file2\n");
 printf ("---------------------------------------\n");
 for (i = 0; i < table->nobjs; i++)
 {
     c1 = (table->objs[i].flags[0]) ? 'x' : ' ';
     c2 = (table->objs[i].flags[1]) ? 'x' : ' ';
     printf ("%5c %6c    %-15s\n", c1, c2, table->objs[i].name);
 }
 printf ("\n");
    }


    /*-------------------------------------------------------------------------
     * do the diff for common objects
     *-------------------------------------------------------------------------
     */
    {
#ifdef H5_HAVE_PARALLEL
	char* workerTasks = malloc((g_nTasks-1) * sizeof(char));
	int n;
	int busyTasks=0;
	struct diffs_found nFoundbyWorker;
	struct diff_args args;
	int havePrintToken = 1;
	MPI_Status Status;

	/*set all tasks as free */
	memset(workerTasks, 1, g_nTasks-1);
#endif

 for (i = 0; i < table->nobjs; i++)
 {
     if (table->objs[i].flags[0] && table->objs[i].flags[1])
     {
#ifdef H5_HAVE_PARALLEL
  int workerFound = 0;
#endif /* H5_HAVE_PARALLEL */
  options->cmn_objs = 1;
  if(!g_Parallel)
  {
      nfound += diff (file1_id,
       table->objs[i].name,
       file2_id,
       table->objs[i].name, options, table->objs[i].type);
  }
#ifdef H5_HAVE_PARALLEL
  else
  {
h5diffdebug("beginning of big else block\n");
      /* We're in parallel mode */
      /* Since the data type of diff value is hsize_t which can
       * be arbitary large such that there is no MPI type that
       * matches it, the value is passed between processes as
       * an array of bytes in order to be portable.  But this
       * may not work in non-homogeneous MPI environments.
       */

      /*Set up args to pass to worker task. */
      if(strlen(table->objs[i].name) > 255)
      {
	  printf("The parallel diff only supports object names up to 255 characters\n");
	  MPI_Abort(MPI_COMM_WORLD, 0);
      }

      strcpy(args.name, table->objs[i].name);
      args.options = *options;
      args.type= table->objs[i].type;

h5diffdebug2("busyTasks=%d\n", busyTasks);
      /* if there are any outstanding print requests, let's handle one. */
      if(busyTasks > 0)
      {
	  int incomingMessage;
	  /* check if any tasks freed up, and didn't need to print. */
	  MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &incomingMessage, &Status);

	  /* first block*/
	  if(incomingMessage)
	  {
	      workerTasks[Status.MPI_SOURCE-1] = 1;
	      MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
	      nfound += nFoundbyWorker.nfound;
	      options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
	      busyTasks--;
	  }

	  /* check to see if the print token was returned. */
	  if(!havePrintToken)
	  {
	      /* If we don't have the token, someone is probably sending us output */
	      print_incoming_data();

	      /* check incoming queue for token */
	      MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

	      /* incoming token implies free task. */
	      if(incomingMessage)
	      {
		  workerTasks[Status.MPI_SOURCE-1] = 1;
		  MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
		  nfound += nFoundbyWorker.nfound;
		  options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		  busyTasks--;
		  havePrintToken = 1;
	      }
	  }

	  /* check to see if anyone needs the print token. */
	  if(havePrintToken)
	  {
	      /* check incoming queue for print token requests */
	      MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &incomingMessage, &Status);
	      if(incomingMessage)
	      {
		  MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
		  MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
		  havePrintToken = 0;
	      }
	  }
      }

      /* check array of tasks to see which ones are free.
       * Manager task never does work, so freeTasks[0] is really
       * worker task 0. */

      for(n=1; (n<g_nTasks) && !workerFound; n++)
      {
	  if(workerTasks[n-1])
	  {
	      /* send file id's and names to first free worker */
	      MPI_Send(&args, sizeof(struct diff_args), MPI_BYTE, n, MPI_TAG_ARGS, MPI_COMM_WORLD);

	      /* increment counter for total number of prints. */
	      busyTasks++;

	      /* mark worker as busy */
	      workerTasks[n-1] = 0;
	      workerFound = 1;
	  }

      }


 h5diffdebug2("workerfound is %d \n", workerFound);
      if(!workerFound)
      {
	  /* if they were all busy, we've got to wait for one free up before we can move on.
	   * if we don't have the token, some task is currently printing so we'll wait for that task to return it. */

	  if(!havePrintToken)
	  {
	      while(!havePrintToken) {
		  int incomingMessage;
		  print_incoming_data();
		  MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);
		  if(incomingMessage)
		  {
		      MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
		      havePrintToken = 1;
		      nfound += nFoundbyWorker.nfound;
		      options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		      /* send this task the work unit. */
		      MPI_Send(&args, sizeof(struct diff_args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
		  }
	      }
	  }
   /* if we do have the token, check for task to free up, or wait for a task to request it */
	  else
	  {
	      /* But first print all the data in our incoming queue */
	      print_incoming_data(); 
	      MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
	      if(Status.MPI_TAG == MPI_TAG_DONE)
	      {
		  MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
		  nfound += nFoundbyWorker.nfound;
		  options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		  MPI_Send(&args, sizeof(struct diff_args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
	      }
	      else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
	      {
		  int incomingMessage;
		  MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
		  MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

		  do
		  {
		      MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

		      print_incoming_data();
		  }
		  while(!incomingMessage);


		  MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
		  nfound += nFoundbyWorker.nfound;
		  options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		  MPI_Send(&args, sizeof(struct diff_args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
	      }
	      else
	      {
		  printf("ERROR: Invalid tag (%d) received \n", Status.MPI_TAG);
		  MPI_Abort(MPI_COMM_WORLD, 0);
		  MPI_Finalize();
	      }
	  }
      }
  }
#endif /* H5_HAVE_PARALLEL */
     }
 }
 h5diffdebug("done with for loop\n");

#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
 {
     while(busyTasks > 0) /* make sure all tasks are done */
     {
	 MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
	 if(Status.MPI_TAG == MPI_TAG_DONE)
	 {
	     MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
	     nfound += nFoundbyWorker.nfound;
	     options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
	     busyTasks--;
	 }
	 else if(Status.MPI_TAG == MPI_TAG_TOK_RETURN)
	 {
	     MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
	     nfound += nFoundbyWorker.nfound;
	     options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
	     busyTasks--;
	     havePrintToken = 1;
	 }
	 else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
	 {
	     MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
	     if(havePrintToken)
	     {
		 int incomingMessage;
		 MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
		 do
		 {
		     MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

		     print_incoming_data();
		 }
		 while(!incomingMessage);
		 MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
		 nfound += nFoundbyWorker.nfound;
		 options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		 busyTasks--;
	     }
	     else /* someone else must have it...wait for them to return it, then give it to the task that just asked for it. */
	     {
		 int source = Status.MPI_SOURCE;
		 int incomingMessage;
		 do
		 {
		     MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

		     print_incoming_data();
		 }
		 while(!incomingMessage);


		 MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
		 nfound += nFoundbyWorker.nfound;
		 options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
		 busyTasks--;
		 MPI_Send(NULL, 0, MPI_BYTE, source, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
	     }
	 }
	 else if(Status.MPI_TAG == MPI_TAG_TOK_RETURN)
	 {
	     MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
	     nfound += nFoundbyWorker.nfound;
	     options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
	     busyTasks--;
	     havePrintToken = 1;
	 }
	 else if(Status.MPI_TAG == MPI_TAG_PRINT_DATA)
	 {
	     char  data[PRINT_DATA_MAX_SIZE+1];
	     memset(data, 0, PRINT_DATA_MAX_SIZE+1);

	     MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

	     printf("%s", data);
	 }
	 else
	 {
	     printf("ph5diff-manager: ERROR!! Invalid tag (%d) received \n", Status.MPI_TAG);
	     MPI_Abort(MPI_COMM_WORLD, 0);
	 }
     }

     for(i=1; i<g_nTasks; i++)
	 MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);

     /* Print any final data waiting in our queue */
     print_incoming_data();

 }
h5diffdebug("done with if block\n");

 free(workerTasks);
#endif /* H5_HAVE_PARALLEL */
    }
    /* free table */
    trav_table_free (table);


    /*-------------------------------------------------------------------------
     * do the diff for the root.
     * this is a special case, we get an ID for the root group and call diff()
     * with this ID; it compares only the root group attributes
     *-------------------------------------------------------------------------
     */

    /* the manager can do this. */
    nfound += diff (file1_id, "/", file2_id, "/", options, H5G_GROUP);

    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_compare
 *
 * Purpose: get objects from list, and check for the same type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t
diff_compare (hid_t file1_id,
 const char *file1_name,
 const char *obj1_name,
 int nobjects1,
 trav_info_t * info1,
 hid_t file2_id,
 const char *file2_name,
 const char *obj2_name,
 int nobjects2, trav_info_t * info2, diff_opt_t * options)
{

    int f1 = 0, f2 = 0;
    hsize_t nfound = 0;

    int i = h5trav_getindex (obj1_name, nobjects1, info1);
    int j = h5trav_getindex (obj2_name, nobjects2, info2);

    if (i == -1)
    {
 parallel_print ("Object <%s> could not be found in <%s>\n", obj1_name,
  file1_name);
 f1 = 1;
    }
     if (j == -1)
     {
  parallel_print ("Object <%s> could not be found in <%s>\n", obj2_name,
   file2_name);
  f2 = 1;
     }
     if (f1 || f2)
     {
  options->err_stat = 1;
  return 0;
     }

     /* use the name with "/" first, as obtained by iterator function */
     obj1_name = info1[i].name;
  obj2_name = info2[j].name;

  /* objects are not the same type */
  if (info1[i].type != info2[j].type)
    {
      if (options->m_verbose)
 parallel_print
   ("Comparison not possible: <%s> is of type %s and <%s> is of type %s\n",
    obj1_name, get_type (info1[i].type), obj2_name,
    get_type (info2[j].type));
    options->not_cmp=1;
      return 0;
    }

  nfound =
    diff (file1_id, obj1_name, file2_id, obj2_name, options, info1[i].type);

  return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 * TYPE is either
 *  H5G_LINK     Object is a symbolic link
 *  H5G_GROUP    Object is a group
 *  H5G_DATASET  Object is a dataset
 *  H5G_TYPE     Object is a named data type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t
diff (hid_t file1_id,
      const char *path1,
      hid_t file2_id, const char *path2, diff_opt_t * options, H5G_obj_t type)
{
    hid_t       type1_id=(-1);
    hid_t       type2_id=(-1);
    hid_t       grp1_id=(-1);
    hid_t       grp2_id=(-1);
    int ret;
    H5G_stat_t sb1;
    H5G_stat_t sb2;
    char *buf1 = NULL;
    char *buf2 = NULL;
    hsize_t nfound = 0;

    switch (type)
    {
 /*-------------------------------------------------------------------------
  * H5G_DATASET
  *-------------------------------------------------------------------------
  */
    case H5G_DATASET:

 /*-------------------------------------------------------------------------
  * verbose, always print name
  *-------------------------------------------------------------------------
  */
     if (options->m_verbose)
     {
      if (print_objname (options, (hsize_t)1))
       parallel_print("Dataset:     <%s> and <%s>\n", path1, path2);
      nfound = diff_dataset (file1_id, file2_id, path1, path2, options);
      /* always print the number of differences found */
      print_found(nfound);
     }
 /*-------------------------------------------------------------------------
  * non verbose, check first if we have differences
  *-------------------------------------------------------------------------
  */
     else
     {
      if (options->m_quiet == 0)
      {
       /* shut up temporarily */
       options->m_quiet = 1;
       nfound = diff_dataset (file1_id, file2_id, path1, path2, options);
       /* print again */
       options->m_quiet = 0;
       if (nfound)
       {
        if (print_objname (options, nfound))
         parallel_print("Dataset:     <%s> and <%s>\n", path1, path2);
        nfound = diff_dataset (file1_id, file2_id, path1, path2, options);
        /* print the number of differences found only when found
           this is valid for the default mode and report mode */
        print_found(nfound);
       }  /*if nfound */
      }   /*if quiet */

 /*-------------------------------------------------------------------------
  * quiet mode, just count differences
  *-------------------------------------------------------------------------
  */
      else
      {
       nfound = diff_dataset (file1_id, file2_id, path1, path2, options);
      }
     }   /*else verbose */

     break;

     /*-------------------------------------------------------------------------
      * H5G_TYPE
      *-------------------------------------------------------------------------
      */
 case H5G_TYPE:
     if ((type1_id = H5Topen (file1_id, path1)) < 0)
  goto out;
     if ((type2_id = H5Topen (file2_id, path2)) < 0)
  goto out;

     if ((ret = H5Tequal (type1_id, type2_id)) < 0)
  goto out;

     /* if H5Tequal is > 0 then the datatypes refer to the same datatype */
     nfound = (ret > 0) ? 0 : 1;

     if (print_objname (options, nfound))
      parallel_print("Datatype:    <%s> and <%s>\n", path1, path2);

     /* always print the number of differences found in verbose mode */
     if (options->m_verbose)
      print_found(nfound);

     /*-------------------------------------------------------------------------
      * compare attributes
      * the if condition refers to cases when the dataset is a referenced object
      *-------------------------------------------------------------------------
      */
         if (path1)
    diff_attr (type1_id, type2_id, path1, path2, options);

     if (H5Tclose (type1_id) < 0)
  goto out;
     if (H5Tclose (type2_id) < 0)
  goto out;

     break;

     /*-------------------------------------------------------------------------
      * H5G_GROUP
      *-------------------------------------------------------------------------
      */
 case H5G_GROUP:
     if ((grp1_id = H5Gopen (file1_id, path1)) < 0)
  goto out;
     if ((grp2_id = H5Gopen (file2_id, path2)) < 0)
  goto out;

     ret = HDstrcmp (path1, path2);

     /* if "path1" != "path2" then the groups are "different" */
     nfound = (ret != 0) ? 1 : 0;

     if (print_objname (options, nfound))
      parallel_print("Group:       <%s> and <%s>\n", path1, path2);

     /* always print the number of differences found in verbose mode */
     if (options->m_verbose)
      print_found(nfound);

     /*-------------------------------------------------------------------------
      * compare attributes
      * the if condition refers to cases when the dataset is a referenced object
      *-------------------------------------------------------------------------
      */
     if (path1)
  diff_attr (grp1_id, grp2_id, path1, path2, options);

     if (H5Gclose (grp1_id) < 0)
  goto out;
     if (H5Gclose (grp2_id) < 0)
  goto out;

     break;


     /*-------------------------------------------------------------------------
      * H5G_LINK
      *-------------------------------------------------------------------------
      */
 case H5G_LINK:
     if (H5Gget_objinfo (file1_id, path1, FALSE, &sb1) < 0)
  goto out;
     if (H5Gget_objinfo (file1_id, path1, FALSE, &sb2) < 0)
  goto out;

     buf1 = malloc (sb1.u.slink.linklen);
     buf2 = malloc (sb2.u.slink.linklen);

     if (H5Gget_linkval (file1_id, path1, sb1.u.slink.linklen, buf1) < 0)
  goto out;
     if (H5Gget_linkval (file2_id, path2, sb1.u.slink.linklen, buf2) < 0)
  goto out;

     ret = HDstrcmp (buf1, buf2);

     /* if "buf1" != "buf2" then the links are "different" */
     nfound = (ret != 0) ? 1 : 0;

     if (print_objname (options, nfound))
  parallel_print("Link:        <%s> and <%s>\n", path1, path2);

     if (buf1)
     {
  free (buf1);
  buf1 = NULL;
     }

     if (buf2)
     {
  free (buf2);
  buf2 = NULL;
     }

     break;


 default:
     nfound = 0;
     if (options->m_verbose)
     {
  parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n",
   path1, path2, get_type (type));
  options->not_cmp=1;
     }

     break;
    }


out:

    /* close */
    /* disable error reporting */
    H5E_BEGIN_TRY
    {
 H5Tclose (type1_id);
 H5Tclose (type2_id);
 H5Gclose (grp1_id);
 H5Tclose (grp2_id);
 /* enable error reporting */
    }
    H5E_END_TRY;

    if (buf1)
 free (buf1);
    if (buf2)
 free (buf2);

    return nfound;
}


