#!/bin/sh

# This script removes the "EXIT CODE" line in the end of all standard output files.  OpenVMS doesn't output the
# same value as Unix.  So we remove the line first on Unix before running the tests.  Simply run the command
# "sh ./remove_exit.sh" under hdf5/tools/h5diff/testfiles directory.

for file in $(ls *.txt)
do
sed '/EXIT CODE/d' $file > _$file
mv _$file $file
done
