usage: h5clear [OPTIONS] file_name
  OPTIONS
   -h, --help                Print a usage message and exit
   -V, --version             Print version number and exit
   -s, --status              Clear the status_flags field in the file's superblock
   -m, --image               Remove the metadata cache image from the file
   --filesize                Print the file's EOA and EOF
   --increment=C             Set the file's EOA to the maximum of (EOA, EOF) + C for the file <file_name>
                             C is >= 0; C is optional and will default to 1M when not set
Examples of use:

h5clear -s file_name
  Clear the status_flags field in the superblock of the HDF5 file <file_name>.

h5clear -m file_name
  Remove the metadata cache image from the HDF5 file <file_name>.

h5clear --increment file_name
  Set the EOA to the maximum of (EOA, EOF) + 1M for the file <file_name>.

h5clear --increment=512 file_name
  Set the EOA to the maximum of (EOA, EOF) + 512 for the file <file_name>.
