#!/usr/bin/perl
# makeOuterREADME.pl
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
#print OUTFILE $_, "\n";
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

print $indirectory, "\n";
 
if ($indirectory =~ /loyalty/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz   - Includes C and C++ APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C APIs (using\n";
   $compilerstring = "                                        gcc, g++, and gfortran 4.6.1)\n";
}
elsif ($indirectory =~ /freedom/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C and C++ APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C APIs (using\n";
   $compilerstring = "                                        gcc, g++, and gfortran 4.6.1)\n";
}
elsif ($indirectory =~ /ostrich/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        gcc, g++, and gfortran 4.4.7)\n";
}
elsif ($indirectory =~ /platypus/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        gcc, g++, and gfortran 4.4.7)\n";
}
elsif ($indirectory =~ /jam/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   if ($indirectory =~ /gnu482/) {
      $compilerstring = "                                        gcc, g++, and gfortran 4.8.2)\n";
   } else {    
      $compilerstring = "                                        gcc, g++, and gfortran 4.1.2)\n";
   }
}
elsif ($indirectory =~ /koala/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   if ($indirectory =~ /gnu482/) {
      $compilerstring = "                                        gcc, g++, and gfortran 4.8.2)\n";
   } else {    
      $compilerstring = "                                        gcc, g++, and gfortran 4.1.2)\n";
   }
} 
elsif ($indirectory =~ /emu/) {
#   $line1 = "  5-$version-$outsubdir-16API.tar.gz - Includes C, C++, F90 APIs (using\n";
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        Sun C and C++ 5.12, Sun Fortran 95 8.6)\n";
}
elsif ($indirectory =~ /duck/) {
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        Apple clang/clang++ 3.0 from Xcode 4.6.1, 
                                                              gfortran 4.6.2)\n";
}
elsif ($indirectory =~ /kite/) {
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        Apple clang/clang++ 5.0 from Xcode 5.0.2,
                                                              gfortran 4.8.2)\n";
}
elsif ($indirectory =~ /quail/) {
   $line3 = "  5-$version-$outsubdir-static.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $line5 = "  5-$version-$outsubdir-shared.tar.gz  - Includes C, C++, F90 APIs (using\n";
   $compilerstring = "                                        Apple clang/clang++ 5.1 from Xcode 5.1,
                                                              gfortran 4.8.2)\n";
}

    print OUTFILE $line3;
    print OUTFILE $compilerstring."\n";
if ($line5 ne "") {
    print OUTFILE $line5;
    print OUTFILE $compilerstring."\n";
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
