#############################
Expected output for 'h5dump '
#############################
usage: h5dump [OPTIONS] file
  OPTIONS
     -h, --help           Print a usage message and exit
     -n, --contents       Print a list of the file contents and exit
     -B, --bootblock      Print the content of the boot block
     -H, --header         Print the header only; no data is displayed
     -A                   Print the header and value of attributes; data of datasets is not displayed
     -i, --object-ids     Print the object ids
     -r, --string         Print 1-byte integer datasets as ASCII
     -e,                  Escape non printing characters
     -V, --version        Print version number and exit
     -a P, --attribute=P  Print the specified attribute
     -d P, --dataset=P    Print the specified dataset
     -y                   Do not print array indices with the data
     -p,   --properties   Print dataset filters, storage layout and fill value
     -f D, --filedriver=D Specify which driver to open the file with
     -g P, --group=P      Print the specified group and all members
     -l P, --soft-link=P  Print the value(s) of the specified soft link
     -o F, --output=F     Output raw data into file F
     -b F                 Output raw data into file F in binary form (use with -d)
     -F T                 Form of binary output (T can be NA for native type, DI for the disk file type, LI or BI for pre-existing little or big endian types)
     -t P, --datatype=P   Print the specified named data type
     -w N, --width=N      Set the number of columns of output
     -x, --xml            Output in XML using Schema
     -u, --use-dtd        Output in XML using DTD
     -D U, --xml-dtd=U    Use the DTD or schema at U
     -X S, --xml-ns=S      (XML Schema) Use qualified names n the XML
                          ":": no namespace, default: "hdf5:"
                          E.g., to dump a file called `-f', use h5dump -- -f

 Subsetting is available by using the following options with a dataset
 attribute. Subsetting is done by selecting a hyperslab from the data.
 Thus, the options mirror those for performing a hyperslab selection.
 The START and COUNT parameters are mandatory if you do subsetting.
 The STRIDE and BLOCK parameters are optional and will default to 1 in
 each dimension.

      -s L, --start=L     Offset of start of subsetting selection
      -S L, --stride=L    Hyperslab stride
      -c L, --count=L     Number of blocks to include in selection
      -k L, --block=L     Size of block in hyperslab

  D - is the file driver to use in opening the file. Acceptable values
        are "sec2", "family", "split", "multi", and "stream". Without
        the file driver flag, the file will be opened with each driver in
        turn and in the order specified above until one driver succeeds
        in opening the file.
  F - is a filename.
  P - is the full path from the root group to the object.
  N - is an integer greater than 1.
  L - is a list of integers the number of which are equal to the
        number of dimensions in the dataspace being queried
  U - is a URI reference (as defined in [IETF RFC 2396],
        updated by [IETF RFC 2732])

  Examples:

  1) Attribute foo of the group /bar_none in file quux.h5

     	h5dump -a /bar_none/foo quux.h5

  2) Selecting a subset from dataset /foo in file quux.h5

      h5dump -d /foo -s "0,1" -S "1,1" -c "2,3" -k "2,2" quux.h5

h5dump error: missing file name
