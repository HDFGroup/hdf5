usage: h5format_convert [OPTIONS] file_name
  OPTIONS
   -h, --help                Print a usage message and exit
   -V, --version             Print version number and exit
   -v, --verbose             Turn on verbose mode
   -d dname, --dname=dataset_name    Pathname for the dataset
   -n, --noop                Perform all the steps except the actual conversion

Examples of use:

h5format_convert -d /group/dataset file_name
  Convert the dataset </group/dataset> in the HDF5 file <file_name>:
    a. chunked dataset: convert the chunk indexing type to version 1 B-tree
    b. compact/contiguous dataset: downgrade the layout version to 3
    c. virtual dataset: no action

h5format_convert file_name
  Convert all datasets in the HDF5 file <file_name>:
    a. chunked dataset: convert the chunk indexing type to version 1 B-tree
    b. compact/contiguous dataset: downgrade the layout version to 3
    c. virtual dataset: no action

h5format_convert -n -d /group/dataset file_name
  Go through all the steps except the actual conversion when 
  converting the dataset </group/dataset> in the HDF5 file <file_name>.
h5format_convert error: missing file name
