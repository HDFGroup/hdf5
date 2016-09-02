#!/usr/bin/perl
# makeOuterREADME.pl

# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF4.  The full HDF4 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF4 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF4/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#

use warnings;
use strict;

my $indirectory = ".";
$indirectory = shift;

my $outdirectory = ".";
$outdirectory = shift;

my $outsubdir = shift;

my $version;

my $outfile = "$outdirectory/$outsubdir/README";
open OUTFILE, ">$outfile";
my $hostname="";
my $cmd = "grep \"HDF5 Version\" $indirectory-static/lib/libhdf5.settings";
$_ = `$cmd`;
print OUTFILE $_, "\n";
s/HDF5 Version://;
s/^\s+//;
chomp;
$version = $_;
#print OUTFILE $_, "\n";
my $versionstring=  "This directory contains the precompiled HDF5 $version binary distribution\n(include files, libraries, utilities) for";

$cmd = "grep \"Uname information:\" $indirectory-static/lib/libhdf5.settings";
$_ = `$cmd`;
s/Uname information://;
s/^\s+//;
chomp;
print "String to work with is $_\n";
my $platformstring = "";
my $hostnamestring = $_;
my @hostnamestring = split / /, $hostnamestring;
$platformstring = "$hostnamestring[0] ";
if ($indirectory =~ /jam/ || $indirectory =~ /koala/) {
   $hostnamestring = $hostnamestring[2];
   my $pos = index $hostnamestring, "-";
   if ($pos > 0) {
      $platformstring .=  substr $hostnamestring, 0, $pos;
   } else {
      $platformstring .= $hostnamestring[2];
   }
   $platformstring .= " ".$hostnamestring[-3];
} elsif ($indirectory =~ /linew/) {
   $platformstring .= "$hostnamestring[2] $hostnamestring[-2]";
} else {
   $platformstring .= "$hostnamestring[2] $hostnamestring[-1]";
}

print OUTFILE $versionstring." ".$platformstring.":\n\n";
my $line1;
my $line3;
my $line5;
my $compilerstring="";
my $compilerstring1="";
my $compilerstring2="";


print $indirectory, "\n";
 
if ($indirectory =~ /ostrich/) {
#   $line1 = "  hdf5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " gcc, g++, and gfortran 4.4.7)";
}
elsif ($indirectory =~ /platypus/) {
#   $line1 = "  hdf5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring1 = " gcc, g++, and gfortran 4.4.7)\n";
   if ($indirectory =~ /32/) {
      $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 and Java APIs (using";
      $compilerstring2 = " gcc, g++, and gfortran 4.4.7)\n";
   } else {
      $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 and Java APIs (using";
      $compilerstring2 = " gcc, g++, and gfortran 4.4.7 and java 1.8.0_51)\n";
   }
}
elsif ($indirectory =~ /moohan/) {
#   $line1 = "  hdf5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring1 = " gcc, g++, and gfortran 4.8.5)\n";
   if ($indirectory =~ /32/) {
      $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 and Java APIs (using";
      $compilerstring2 = " gcc, g++, and gfortran 4.4.7)\n";
   } else {
      $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 and Java APIs (using";
      $compilerstring2 = " gcc, g++, and gfortran 4.8.5 and java 1.8.0_51)\n";
   }
}
elsif ($indirectory =~ /emu/) {
#   $line1 = "  hdf5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " Sun C and C++ 5.12, Sun Fortran 95 8.6)\n";
}
elsif ($indirectory =~ /kite/) {
   $line3 = "  hfd5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hfd5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " Apple clang/clang++ 5.1 from Xcode 5.0.2,
                                                              gfortran 4.8.2)\n";
}
elsif ($indirectory =~ /quail/) {
   $line3 = "  hfd5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hfd5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " Apple clang/clang++ 6.0 from Xcode 5.1,
                                                              gfortran 4.9.2)\n";
}
elsif ($indirectory =~ /osx1010test/) {
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " Apple clang/clang++ 6.1 from Xcode 6.1,
                                                              gfortran 4.9.2)\n";
}
elsif ($indirectory =~ /osx1011test/) {
   $line3 = "  hdf5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using";
   $line5 = "  hdf5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using";
   $compilerstring = " Apple clang/clang++ 7.0.2 from Xcode 7.0,
                                                              gfortran 5.2.0)\n";
}

    print OUTFILE $line3;
    print OUTFILE $compilerstring1."\n";
if ($line5 ne "") {
    print OUTFILE $line5;
    print OUTFILE $compilerstring2."\n";
}
#elsif ($indirectory =~ /-16API/) {
#    print OUTFILE $line1;
#    print OUTFILE $compilerstring;
#    print OUTFILE "                               USES THE VERSION 1.6 DEFAULT API\n";
#    print OUTFILE "                               MAPPING.\n\n";
#}

print OUTFILE "  utilities/ - Directory containing the compiled HDF5 utilities.\n";
print OUTFILE "               These utilities are STATICALLY linked and will run as is.\n\n";
#print OUTFILE "             The tools can be downloaded separately, or ALL in one\n";
#print OUTFILE "             compressed file (5-$version-$outsubdir-static-util.tar.gz).\n\n";
print OUTFILE "\n";
print OUTFILE "To obtain the HDF5 distribution, 'unzip' and 'untar' the distribution\n";
print OUTFILE "file:\n\n";
print OUTFILE "  gzip -cd <gz file from above> | tar xvf -\n";


#$cmd = "grep \"Configured by:\" $indirectory/$key-static/lib/libhdf5.settings";
#$_ = `$cmd`;
#s/Configured by://;
#s/^\s+//;
#print OUTFILE $_;
#chomp;
#my $hostnamestring = $_;
#s/(^\w+)(\s)(\S+)/$1/;
#s/(^.*)(-)(.*)(200[7-8])(.*)(\s)(\S+)/$1 $5/;
#my $platformstring = $_ . ":\n\n";
#my @hostnamestring = split /@/, $hostnamestring;
#print "Size of hostnamestring is ", scalar @hostnamestring, "\n";
#print $hostnamestring[0] . "\t" . $hostnamestring[2]."\t".$hostnamestring[19]."\n";
#my $platformstring = $hostnamestring[1].":\n\n";
#$hostnamestring = $hostnamestring[1];
#my $pos = index $hostnamestring, ".";
#if ($pos > 0) {
#   @hostnamestring = split /\./, $hostnamestring;
#   $platformstring = $hostnamestring[0].":\n\n";
#}
