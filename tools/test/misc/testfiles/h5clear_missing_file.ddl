usage: h5clear [OPTIONS] file_name
  OPTIONS
   -h, --help                Print a usage message and exit
   -V, --version             Print version number and exit
   -s, --status              Clear the status_flags field in the file's superblock
   -m, --image               Remove the metadata cache image from the file

Examples of use:

h5clear -s file_name
  Clear the status_flags field in the superblock of the HDF5 file <file_name>.

h5clear -m file_name
  Remove the metadata cache image from the HDF5 file <file_name>.
h5clear error: missing file name
