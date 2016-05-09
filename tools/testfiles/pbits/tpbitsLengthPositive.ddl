usage: h5dump [OPTIONS] files
  OPTIONS
     -h,   --help         Print a usage message and exit
     -V,   --version      Print version number and exit
--------------- File Options ---------------
     -n,   --contents     Print a list of the file contents and exit
                          Optional value 1 also prints attributes.
     -B,   --superblock   Print the content of the super block
     -H,   --header       Print the header only; no data is displayed
     -f D, --filedriver=D Specify which driver to open the file with
     -o F, --output=F     Output raw data into file F
     -b B, --binary=B     Binary file output, of form B
     -O F, --ddl=F        Output ddl text into file F
                          Use blank(empty) filename F to suppress ddl display
--------------- Object Options ---------------
     -a P, --attribute=P  Print the specified attribute
                          If an attribute name contains a slash (/), escape the
                          slash with a preceding backslash (\).
                          (See example section below.)
     -d P, --dataset=P    Print the specified dataset
     -g P, --group=P      Print the specified group and all members
     -l P, --soft-link=P  Print the value(s) of the specified soft link
     -t P, --datatype=P   Print the specified named datatype
     -N P, --any_path=P   Print any attribute, dataset, group, datatype, or link that matches P
                          P can be the absolute path or just a relative path.
     -A,   --onlyattr     Print the header and value of attributes
                          Optional value 0 suppresses printing attributes.
     --vds-view-first-missing Set the VDS bounds to first missing mapped elements.
     --vds-gap-size=N     Set the missing file gap size, N=non-negative integers
--------------- Object Property Options ---------------
     -i,   --object-ids   Print the object ids
     -p,   --properties   Print dataset filters, storage layout and fill value
     -M L, --packedbits=L Print packed bits as unsigned integers, using mask
                          format L for an integer dataset specified with
                          option -d. L is a list of offset,length values,
                          separated by commas. Offset is the beginning bit in
                          the data value and length is the number of bits of
                          the mask.
     -R,   --region       Print dataset pointed by region references
--------------- Formatting Options ---------------
     -e,   --escape       Escape non printing characters
     -r,   --string       Print 1-byte integer datasets as ASCII
     -y,   --noindex      Do not print array indices with the data
     -m T, --format=T     Set the floating point output format
     -q Q, --sort_by=Q    Sort groups and attributes by index Q
     -z Z, --sort_order=Z Sort groups and attributes by order Z
     --enable-error-stack Prints messages from the HDF5 error stack as they
                          occur.
     --no-compact-subset  Disable compact form of subsetting and allow the use
                          of "[" in dataset names.
     -w N, --width=N      Set the number of columns of output. A value of 0 (zero)
                          sets the number of columns to the maximum (65535).
                          Default width is 80 columns.
--------------- XML Options ---------------
     -x,   --xml          Output in XML using Schema
     -u,   --use-dtd      Output in XML using DTD
     -D U, --xml-dtd=U    Use the DTD or schema at U
     -X S, --xml-ns=S     (XML Schema) Use qualified names n the XML
                          ":": no namespace, default: "hdf5:"
                          E.g., to dump a file called `-f', use h5dump -- -f

--------------- Subsetting Options ---------------
 Subsetting is available by using the following options with a dataset
 option. Subsetting is done by selecting a hyperslab from the data.
 Thus, the options mirror those for performing a hyperslab selection.
 One of the START, COUNT, STRIDE, or BLOCK parameters are mandatory if you do subsetting.
 The STRIDE, COUNT, and BLOCK parameters are optional and will default to 1 in
 each dimension. START is optional and will default to 0 in each dimension.

      -s START,  --start=START    Offset of start of subsetting selection
      -S STRIDE, --stride=STRIDE  Hyperslab stride
      -c COUNT,  --count=COUNT    Number of blocks to include in selection
      -k BLOCK,  --block=BLOCK    Size of block in hyperslab
  START, COUNT, STRIDE, and BLOCK - is a list of integers the number of which are equal to the
      number of dimensions in the dataspace being queried
      (Alternate compact form of subsetting is described in the Reference Manual)

--------------- Option Argument Conventions ---------------
  D - is the file driver to use in opening the file. Acceptable values
      are "sec2", "family", "split", "multi", "direct", and "stream". Without
      the file driver flag, the file will be opened with each driver in
      turn and in the order specified above until one driver succeeds
      in opening the file.
      See examples below for family, split, and multi driver special file name usage.

  F - is a filename.
  P - is the full path from the root group to the object.
  N - is an integer greater than 1.
  T - is a string containing the floating point format, e.g '%.3f'
  U - is a URI reference (as defined in [IETF RFC 2396],
        updated by [IETF RFC 2732])
  B - is the form of binary output: NATIVE for a memory type, FILE for the
        file type, LE or BE for pre-existing little or big endian types.
        Must be used with -o (output file) and it is recommended that
        -d (dataset) is used. B is an optional argument, defaults to NATIVE
  Q - is the sort index type. It can be "creation_order" or "name" (default)
  Z - is the sort order type. It can be "descending" or "ascending" (default)

--------------- Examples ---------------

  1) Attribute foo of the group /bar_none in file quux.h5

      h5dump -a /bar_none/foo quux.h5

     Attribute "high/low" of the group /bar_none in the file quux.h5

      h5dump -a "/bar_none/high\/low" quux.h5

  2) Selecting a subset from dataset /foo in file quux.h5

      h5dump -d /foo -s "0,1" -S "1,1" -c "2,3" -k "2,2" quux.h5

  3) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin'
        using a little-endian type

      h5dump -d /dset -b LE -o out.bin quux.h5

  4) Display two packed bits (bits 0-1 and bits 4-6) in the dataset /dset

      h5dump -d /dset -M 0,1,4,3 quux.h5

  5) Dataset foo in files file1.h5 file2.h5 file3.h5

      h5dump -d /foo file1.h5 file2.h5 file3.h5

  6) Dataset foo in split files splitfile-m.h5 splitfile-r.h5

      h5dump -d /foo -f split splitfile

  7) Dataset foo in multi files mf-s.h5, mf-b.h5, mf-r.h5, mf-g.h5, mf-l.h5 and mf-o.h5

      h5dump -d /foo -f multi mf

  8) Dataset foo in family files fam00000.h5 fam00001.h5 and fam00002.h5

      h5dump -d /foo -f family fam%05d.h5

h5dump error: Packed Bit length value(0) must be positive.
