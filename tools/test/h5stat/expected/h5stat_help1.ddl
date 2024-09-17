usage: h5stat [OPTIONS] file

  ERROR
   --enable-error-stack  Prints messages from the HDF5 error stack as they occur
                         Optional value 2 also prints file open errors
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
   -s, --freespace       Print free space information
   -S, --summary         Print summary of file space information
   --page-buffer-size=N  Set the page buffer cache size, N=non-negative integers
   --s3-cred=<cred>      Access file on S3, using provided credential
                         <cred> :: (region,id,key)
                         If <cred> == "(,,)", no authentication is used.
   --hdfs-attrs=<attrs>  Access a file on HDFS with given configuration
                         attributes.
                         <attrs> :: (<namenode name>,<namenode port>,
                                     <kerberos cache path>,<username>,
                                     <buffer size>)
                         If an attribute is empty, a default value will be
                         used.
   --vol-value           Value (ID) of the VOL connector to use for opening the
                         HDF5 file specified
   --vol-name            Name of the VOL connector to use for opening the
                         HDF5 file specified
   --vol-info            VOL-specific info to pass to the VOL connector used for
                         opening the HDF5 file specified
                         If none of the above options are used to specify a VOL, then
                         the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,
                         if that environment variable is unset) will be used
   --vfd-value           Value (ID) of the VFL driver to use for opening the
                         HDF5 file specified
   --vfd-name            Name of the VFL driver to use for opening the
                         HDF5 file specified
   --vfd-info            VFD-specific info to pass to the VFL driver used for
                         opening the HDF5 file specified
