Usage: h5stat [OPTIONS] file

      OPTIONS
     -h, --help            Print a usage message and exit
     -V, --version         Print version number and exit
     -f, --file            Print file information
     -F, --filemetadata    Print file space information for file's metadata
     -g, --group           Print group information
     -l N, --links=N       Set the threshold for the # of links when printing
                           information for small groups.  N is an integer greater
                           than 0.  The default threshold is 10.
     -G, --groupmetadata   Print file space information for groups' metadata
     -d, --dset            Print dataset information
     -m N, --dims=N        Set the threshold for the dimension sizes when printing
                           information for small datasets.  N is an integer greater
                           than 0.  The default threshold is 10.
     -D, --dsetmetadata    Print file space information for datasets' metadata
     -T, --dtypemetadata   Print datasets' datatype information
     -A, --attribute       Print attribute information
     -a N, --numattrs=N    Set the threshold for the # of attributes when printing
                           information for small # of attributes.  N is an integer greater
                           than 0.  The default threshold is 10.
     -S, --summary         Print summary of file space information
