Notes on parallel repack
========================
December 16, 2021
Meeting with Richard, Elena and Songyu "Ray" Lu
h5repack - Copy an HDF5 file to a new file with or without compression/chunking
------------------------------------------------

Today's discussion focused on a general overview of the initial prototype
of a parallel version of repack. A quick summary of this is discussion follows:

  1. The basic thoughts for this prototype where to utilize MPI ranks
     whenever possible. These opportunities are very dependent on the
     restrictions placed on object creation in the output file.  In
     particular: Objects can either be created by a single rank; or
     collectively.

  2. General opportunities for parllelism to improve performance:
     i) independent processing for dataset copying, i.e. large numbers
        of datasets in the source file;
     ii) cooperative processing (via hyperslab selections) for
        large datasets, e.g. datasets over 1M elements.

  3. The current prototype uses independent processing as much a possible
     and assigns MPI rank 0 with the task of creating all new objects
     in the output file, e.g.
       All groups,
       All datatypes,
       All links,
       All datasets (containers only)

     Independent processes will copy data from an input dataset
     to an output dataset that has already been created in the output file.

  Q1. Which approach for object creation does the prototype follow?
  A1. All MPI ranks read the input file to collect the set of objects.
      We currently assign all object creation tasks (in the output file)
      to MPI rank 0.  Dataset copying is accomplished in parallel
      by having each rank select an object from the table of objects.
      Selection is: object_index modulo MPI(size) == MPI(rank)
      Example:  For 20 datasets and 4 MPI ranks:
        rank 0: object indices{0, 4, 8, 12, 16}
        rank 1: object indices{1, 5, 9, 13, 17}
        rank 2: object indices{2, 6, 10,15, 18}
        rank 3: object indices{3, 7, 11,16, 19}

  Q2. (Elena) Won't having all mpi ranks read the input file cause
      a bit slowdown?  This as opposed to MPI rank 0 read the set of
      objects and then broadcast?
  A2. For application runs that are moderate in size, e.g. < 1000 cores
      this shouldn't be an issue.  If yes, then we can implement the
      read by rank 0 and broadcast.

  Q3. (Ray) Because the current approach only allows MPI rank 0
      to create objects, won't performance suffer for HDF5 files
      which have large numbers of groups?

  A3. YES! But until we have a way to independently create objects,
      then there is no solution for this issue.

  Q4. (Elena) You are currently copying attributes along with
      raw data in the independent MPI ranks.  Will this an issue
      when an attribute is not a fixed size?
  A5. Possibly.. I don't have an answer at this time.

  Q5. Can you collectively copy data using hyperslab selections
      rather than having each MPI rank copy the data from the
      input dataset to the ouput dataset?
  A5. Probably yes.  I can look at implementing this improvement
      in the current design.

     
     
