h5clear clears superblock status flag field, removes metadata cache image, prints
EOA and EOF, or sets EOA of a file.  It is not a general repair tool and should not
be used to fix file corruption.  If a process doesn't shut down cleanly, the
superblock mark can be left that prevents opening a file without SWMR.  Then,
h5clear can be used to remove this superblock mark so that the file can be inspected
and appropriate actions can be taken.

usage: h5clear [OPTIONS] file_name
  OPTIONS
   -h, --help                Print a usage message and exit
   -V, --version             Print version number and exit
   -s, --status              Clear the status_flags field in the file's superblock
   -m, --image               Remove the metadata cache image from the file
   --filesize                Print the file's EOA and EOF
   --increment=C             Set the file's EOA to the maximum of (EOA, EOF) + C for
                             the file <file_name>.
                             C is >= 0; C is optional and will default to 1M when not set.
                             This option helps to repair a crashed SWMR file when the stored
                             EOA in the superblock is different from the actual EOF.
                             The file's EOA and EOF will be the same after applying
                             this option to the file.

Examples of use:

h5clear -s file_name
  Clear the status_flags field in the superblock of the HDF5 file <file_name>.

h5clear -m file_name
  Remove the metadata cache image from the HDF5 file <file_name>.

h5clear --increment file_name
  Set the EOA to the maximum of (EOA, EOF) + 1M for the file <file_name>.

h5clear --increment=512 file_name
  Set the EOA to the maximum of (EOA, EOF) + 512 for the file <file_name>.
