#!/usr/bin/perl -w
require 5.003;

#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#
# Robb Matzke, matzke@llnl.gov
# 30 Aug 1997
#
# Purpose: Given the names of C source files this script will print the
#	   file name, line number, and function name of any function that
#	   doesn't begin with the letter `h' or `H' as stipulated by the
#	   HDF5 programming style guide.
#
#	   Emacs users can run this script as the compile command and
#	   use `next-error' (usually bound to M-`) to find each name
#	   violation.

if(<>) {
    if($ARGV =~ /\//) {
        ($filename) = ($ARGV =~ /^.*\/([A-Za-z0-9_]*)\.c$/);
    } else {
        ($filename) = ($ARGV =~ /([A-Za-z0-9_]*)\.c$/);
    }

    if($filename =~ /H5FDmulti|H5FDstdio/) {
        print "$ARGV is exempt from using Standard library macro wrappers\n";
    } else {
        while (<>) {

           # Get rid of comments by removing the inside part.
           s|/\*.*?\*/||g;
           if ($in_comment) {
              if (/\*\//) {
                 s|.*?\*/||;
                 $in_comment = 0;
              } else {
                 $_="\n";
              }
           } elsif (m|/\*|) {
              s|/\*.*||;
              $in_comment = 1;
           }

           # Get rid of string constants if they begin and end on this line.
           s/([\'\"])([^\1]|\\\1)*?\1/$1$1/g;

           # Get rid of preprocessor directives
           s/^\#.*//;

           # Skip callbacks invoked as methods in a struct
           next if $_ =~ /\b(\)?->|\.)\(?([a-z_A-Z]\w*)\s*\(/;

           # Now find all function calls on this line which don't start with 'H'
           while (($name)=/\b([a-z_A-GI-Z]\w*)\s*\(/) {
              $_ = $';
              
              # Ignore C statements that look sort of like function
              # calls.
              next if $name =~ /^(if|for|offsetof|return|sizeof|switch|while|void)$/;

              # Ignore things that get misdetected because of the simplified
              # parsing that takes place here.
              next if $name =~ /^(int|herr_t|_term_interface)$/;

              # These are really HDF5 functions/macros even though they don't
              # start with `h' or `H'.
              next if $name =~ /^FUNC_(ENTER|LEAVE)(_(NO)?API|_PACKAGE|_STATIC)?(_NOFS|_NOCLEAR|_NOINIT)?(_NOFUNC|_TAG)?$/;
              next if $name =~ /^(BEGIN|END)_FUNC$/;
              next if $name =~ /^U?INT(8|16|32|64)(ENCODE|DECODE)(_VAR)?$/;
              next if $name =~ /^CI_(PRINT_STATS|INC_SRC|INC_DST)$/;
              next if $name =~ /^(ABS|ADDR_OVERFLOW|ALL_MEMBERS|BOUND|CONSTR|DETECT_[I|F|M]|DOWN)$/;
              next if $name =~ /^(MIN3?|MAX3?|NELMTS|POWER_OF_TWO|REGION_OVERFLOW)$/;
              next if $name =~ /^(UNIQUE_MEMBERS)$/;
              next if $name =~ /^addr_defined$/;

              # These functions/macros are exempt.
              next if $name =~ /^(main|[fs]?printf|va_(start|arg|end))$/;

              # These are Windows system calls. Ignore them.
              next if $name =~ /^(_get_osfhandle|GetFileInformationByHandle|SetFilePointer|GetLastError|SetEndOfFile)$/;
              next if $name =~ /^(FindNextFile|FindClose|_tzset|Wgettimeofday|GetSystemTimeAsFileTime|Wgetlogin|GetUserName)$/;

              # These are MPI function calls. Ignore them.
              next if $name =~ /^(MPI_|MPE_)/;

              # These are POSIX threads function calls. Ignore them.
              next if $name =~ /^pthread_/;

              # These are Windows threads function calls. Ignore them.
              next if $name =~ /^(_beginthread|(Initialize|Enter|Leave)CriticalSection|TlsAlloc)$/;

              # These are zlib & szlib function calls. Ignore them.
              next if $name =~ /^(inflate|SZ_)/;
              next if $name =~ /^compress2$/;

              print "$ARGV:$.: $name\n";
           }

        } continue {
           close ARGV if eof;       # reset line number
        }
    }
}
