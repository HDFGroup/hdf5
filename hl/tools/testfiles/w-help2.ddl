#############################
 output for 'h5watch --help'
#############################
Usage: h5watch [OPTIONS] [OBJECT]

     OPTIONS
        -h, --help            Print a usage message and exit.
        -V, --version         Print version number and exit.
        -l, --label           Label members of compound typed dataset.
        -S, --simple          Use a machine-readable output format.
        -d, --dim             Monitor changes in size of dataset dimensions only.
        -wN, --width=N        Set the number of columns to N for output.
                              A value of 0 sets the number of columns to the
                              maximum (65535). The default width is 80 columns.
        -pN, --polling=N      Set the polling interval to N (in seconds) when the
                              dataset will be checked for appended data.  The default
                              polling interval is 1.
        -f<list_of_fields>, --fields=<list_of_fields>
                              Display data for the fields specified in <list_of_fields>
                              for a compound data type.  <list_of_fields> can be
                              specified as follows:
                                   1) A comma-separated list of field names in a
                                   compound data type.  "," is the separator
                                   for field names while "." is the separator
                                   for a nested field.
                                   2) A single field name in a compound data type.
                                   Can use this option multiple times.
                              Note that backslash is the escape character to avoid
                              characters in field names that conflict with the tool's
                              separators.

     OBJECT is specified as [<filename>/<path_to_dataset>/<dsetname>]
        <filename>            Name of the HDF5 file.  It may be preceded by path
                              separated by slashes to the specified HDF5 file.
        <path_to_dataset>     Path separated by slashes to the specified dataset
        <dsetname>            Name of the dataset

