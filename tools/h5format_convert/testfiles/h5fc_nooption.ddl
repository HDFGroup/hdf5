usage: h5format_convert [OPTIONS] file_name
  OPTIONS
   -h, --help				Print a usage message and exit
   -V, --version			Print version number and exit
   -v, --verbose			Turn on verbose mode
   -d dname, --dname=dataset_name	Pathname for the dataset
   -n, --noop				Perform all the steps except the actual conversion

Examples of use:

h5format_convert -d /group/dataset file_name
  Convert the chunk indexing type to version 1 B-tree
  for the chunked dataset </group/dataset> in the HDF5 file <file_name>.

h5format_convert file_name
  Convert the chunk indexing type to version 1 B-tree
  for all the chunked datasets in the HDF5 file <file_name>.

h5format_convert -n -d /group/dataset file_name
  Go through all the steps except the actual conversion when 
  converting the chunked dataset </group/dataset> in the HDF5 file <file_name>.
