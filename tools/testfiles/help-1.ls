#############################
 output for 'h5ls -w80 -h'
#############################
usage: h5ls [OPTIONS] [OBJECTS...]
   OPTIONS
      -h, -?, --help   Print a usage message and exit
      -a, --address    Print addresses for raw data
      -d, --data       Print the values of datasets
      -e, --errors     Show all HDF5 error reporting
      -f, --full       Print full path names instead of base names
      -g, --group      Show information about a group, not its contents
      -l, --label      Label members of compound datasets
      -r, --recursive  List all groups recursively, avoiding cycles
      -s, --string     Print 1-byte integer datasets as ASCII
      -S, --simple     Use a machine-readable output format
      -wN, --width=N   Set the number of columns of output
      -v, --verbose    Generate more verbose output
      -V, --version    Print version number and exit
      --vfd=DRIVER     Use the specified virtual file driver
      -x, --hexdump    Show raw data in hexadecimal format

   OBJECTS
      Each object consists of an HDF5 file name optionally followed by a
      slash and an object name within the file (if no object is specified
      within the file then the contents of the root group are displayed).
      The file name may include a printf(3C) integer format such as
      "%05d" to open a file family.
