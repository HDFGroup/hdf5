/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		debug.c
 * 			Jul 18 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Debugs an existing HDF5 file at a low level.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include "hdf5.h"

#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5Hprivate.h"
#include "H5Oprivate.h"

#define INDENT	3
#define VCOL	50


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Usage:	debug FILENAME [OFFSET]
 *
 * Return:	Success:	exit (0)
 *
 *		Failure:	exit (non-zero)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
   hatom_t	fid;
   hdf5_file_t	*f;
   haddr_t	addr = 0;
   uint8	sig[16];
   intn		i;
   herr_t	status = SUCCEED;
   haddr_t	extra = 0;

   /*
    * Parse command arguments.
    */
   if (argc>2) {
      printf ("New address: %s\n", argv[2]);
      addr = strtol (argv[2], NULL, 0);
   }
   if (argc>3) {
      extra = strtol (argv[3], NULL, 0);
   }
   
   /*
    * Open the file and get the file descriptor.
    */
   if ((fid = H5Fopen (argv[1], 0, 0))<0) {
      fprintf (stderr, "cannot open file\n");
      exit (1);
   }
   if (NULL==(f=H5Aatom_object (fid))) {
      fprintf (stderr, "cannot obtain hdf5_file_t pointer\n");
      exit (2);
   }

   /*
    * Read the signature at the specified file position.
    */
   printf ("Reading signature at byte %lu\n", (unsigned long)addr);
   if (H5F_block_read (f, addr, sizeof(sig), sig)<0) {
      fprintf (stderr, "cannot read signature\n");
      exit (3);
   }

   if (!memcmp (sig, HDF5_FILE_SIGNATURE, HDF5_FILE_SIGNATURE_LEN)) {
      /*
       * Debug the boot block.
       */
      status = H5F_debug (f, 0, stdout, 0, VCOL);
      
   } else if (!memcmp (sig, H5H_MAGIC, H5H_SIZEOF_MAGIC)) {
      /*
       * Debug a heap.
       */
      status = H5H_debug (f, addr, stdout, 0, VCOL);

   } else if (!memcmp (sig, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) {
      /*
       * Debug a symbol table node.
       */
      status = H5G_node_debug (f, addr, stdout, 0, VCOL, extra);

   } else if (!memcmp (sig, H5B_MAGIC, H5B_SIZEOF_MAGIC)) {
      /*
       * Debug a B-tree.  B-trees are debugged through the B-tree
       * subclass.  The subclass identifier is the byte immediately
       * after the B-tree signature.
       */
      H5B_subid_t subtype = sig[H5B_SIZEOF_MAGIC];
      switch (subtype) {
      case H5B_SNODE_ID:
	 status = H5G_node_debug (f, addr, stdout, 0, VCOL, extra);
	 break;

      default:
	 fprintf (stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
	 exit (4);
      }

   } else if (sig[0]==H5O_VERSION && sig[1]==H5O_ALIGNMENT) {
      /*
       * This could be an object header.  Since they don't have a signature
       * it's a somewhat "ify" detection.
       */
      status = H5O_debug (f, addr, stdout, 0, VCOL);
      
   } else {
      /*
       * Got some other unrecognized signature.
       */
      printf ("%-*s ", VCOL, "Signature:");
      for (i=0; i<8; i++) {
	 if (sig[i]>' ' && sig[i]<='~' && '\\'!=sig[i]) {
	    putchar (sig[i]);
	 } else if ('\\'==sig[i]) {
	    putchar ('\\');
	    putchar ('\\');
	 } else {
	    printf ("\\%03o", sig[i]);
	 }
      }
      putchar ('\n');
      
      fprintf (stderr, "unknown signature\n");
      exit (4);
   }

   if (status<0) {
      fprintf (stderr, "An error occurred\n");
      exit (5);
   }

   H5Fclose (fid);
   exit (0);
}
