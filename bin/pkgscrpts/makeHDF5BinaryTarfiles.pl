#!/usr/bin/perl
# makeTarFiles.pl

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
#
#
use warnings;
use strict;
use Cwd;
use File::Basename;


my %destsubdir = ('emu' => 'sunos-5.11-sparc-32-sunc512',
                  'emu64' => 'sunos-5.11-sparc-64-sunc512',
                  'ostrich' => 'linux-el6-ppc64-gcc447',
                  'ostrichxl' => 'linux-el6-ppc64-xl13.1',
                  'platypus' => 'linux-centos6-x86_64-gcc447',
                  'platypus32' => 'linux-centos6-x86_64-32-gcc447',
                  'moohan' => 'linux-centos7-x86_64-gcc485',
                  'moohan32' => 'linux-centos7-x86_64-32-gcc485',
                  'kite' => 'osx-10.8-x86_64-clang5.1',
                  'quail' => 'osx-10.9-x86_64-clang6.0',
                  'osx1010test' => 'osx-10.10-x86_64-clang6.0');

my %szipdir = ('emu' =>  '/mnt/hdf/packages/szip/shared/encoder/SunOS-5.10',
               'emu-static' => '/mnt/hdf/packages/szip/static/encoder/SunOS-5.10',
               'emu64' => '/mnt/hdf/packages/szip-PIC/shared/encoder/SunOS-5.11-64',
               'emu64-static' => '/mnt/hdf/packages/szip-PIC/shared/encoder/SunOS-5.11-64',
               'kite' => '/mnt/hdf/packages/szip/shared/encoder/MacOS-10.8',
               'kite-static' => '/mnt/hdf/packages/szip-PIC/static/encoder/MacOS-10.8',
               'ostrich32' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-ppc64-gcc',
               'ostrich32-static' => '/mnt/hdf/packages/szip/static/encoder/Linux2.6-ibmppc64-gcc-32',
               'ostrich' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-ppc64-gcc-64',
               'ostrich-static' => '/mnt/hdf/packages/szip/static/encoder/Linux2.6-ibmppc64-gcc',
               'ostrichxl' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-ppc64-gcc-64',
               'ostrichxl-static' => '/mnt/hdf/packages/szip/static/encoder/Linux2.6-ibmppc64-gcc',
               'osx1010test' => '/mnt/hdf/packages/szip/shared/encoder/MacOS-10.8',
               'osx1010test-static' => '/mnt/hdf/packages/szip-PIC/static/encoder/MacOS-10.8',
               'moohan' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-x86_64-gcc',
               'moohan-static' => '/mnt/hdf/packages/szip/static/encoder/Linux2.6-x86_64-gcc',
               'moohan32' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-x86_64-gcc-m32',
               'moohan32-static' => '/mnt/hdf/packages/szip-PIC/static/encoder/Linux2.6-x86_64-gcc-m32',
               'platypus32' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-x86_64-gcc-m32',
               'platypus32-static' => '/mnt/hdf/packages/szip-PIC/static/encoder/Linux2.6-x86_64-gcc-m32',
               'platypus' => '/mnt/hdf/packages/szip/shared/encoder/Linux2.6-x86_64-gcc',
               'platypus-static' => '/mnt/hdf/packages/szip/static/encoder/Linux2.6-x86_64-gcc',
               'quail' => '/mnt/hdf/packages/szip/shared/encoder/MacOS-10.8',
               'quail-static' => '/mnt/hdf/packages/szip-PIC/static/encoder/MacOS-10.8');

my %zlibdir = ('emu' => '/mnt/hdf/packages/zlib-125/shared/SunOS-5.10',
               'emu-static' => '/mnt/hdf/packages/zlib-125/static/SunOS-5.10',
               'emu64' => '/mnt/hdf/packages/zlib-123-PIC/SunOS-5.11-64',
               'emu64-static' => '/mnt/hdf/packages/zlib-123-PIC/SunOS-5.11-64',
               'kite' => ' /mnt/hdf/packages/zlib-125/shared/mac-intel-x86_64',
               'kite-static' => ' /mnt/hdf/packages/zlib-125/static/mac-intel-x86_64',
               'ostrich32' => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc',
               'ostrich32-static' => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc',
               'ostrich' => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc-64',
               'ostrich-static'  => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc-64',
               'ostrichxl' => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc-64',
               'ostrichxl-static'  => '/mnt/hdf/packages/zlib-125/PIC/Linux2.6-ppc64-gcc-64',
               'osx1010test' => ' /mnt/hdf/packages/zlib-125/shared/mac-intel-x86_64',
               'osx1010test-static' => ' /mnt/hdf/packages/zlib-125/static/mac-intel-x86_64',
               'moohan' => '/mnt/hdf/packages/zlib-125/shared/Linux2.6-x86_64-gcc',
               'moohan-static' => '/mnt/hdf/packages/zlib-125/static/Linux2.6-x86_64-gcc',
               'moohan32' => '/mnt/hdf/packages/zlib-128/Linux2.6-x86_64-gcc-m32',
               'moohan32-static' => '/mnt/hdf/packages/zlib-128/Linux2.6-x86_64-gcc-m32',
               'platypus32' => '/mnt/hdf/packages/zlib-128/Linux2.6-x86_64-gcc-m32',
               'platypus32-static' => '/mnt/hdf/packages/zlib-128/Linux2.6-x86_64-gcc-m32',
               'platypus' => '/mnt/hdf/packages/zlib-125/shared/Linux2.6-x86_64-gcc',
               'platypus-static' => '/mnt/hdf/packages/zlib-125/static/Linux2.6-x86_64-gcc',
               'quail' => ' /mnt/hdf/packages/zlib-125/shared/mac-intel-x86_64',
               'quail-static' => ' /mnt/hdf/packages/zlib-125/static/mac-intel-x86_64');

my $indirectory = ".";
$indirectory = shift;

my $outdirectory = ".";
$outdirectory = shift;

my $key = ".";
#$key = shift;

my $scriptdirname = dirname(__FILE__);

unless (-d $outdirectory) {
   print "$outdirectory not found.  Create it or choose another one and try again.\n";
   exit 1;
}
 
print "Subdirectories of $indirectory will be tarred and stored in $outdirectory.\n";


my $tarfilename;
my $tardirname;
my $output;
my $cmd;

# I'm currently copying system zlibs for at least solaris and FreeBSD machines.  Since this script runs on jam it may require scp to get the libs.

#For future reference
# command for getting szlib files and links for shared binaries:
# tar cvf - -C <szipDir>/lib . | tar xvf - -C <libdir> .
# libz.so.1.2.3 and the static files should just be copied because they're in directories
# with other files.  Then create the libz.so.1 and libz.so symbolic links.
sub addzandszlibs {
   my $dir = shift;
   my $indirectory = shift;
   my $currentdir = getcwd();

   if (-d "$indirectory/$dir" ) {
      my $szdir = $szipdir{$dir};
      my $zldir = $zlibdir{$dir};
      if ($dir =~ /static/ || $dir =~ /ostrich/) {
         $cmd = "cp $szdir/lib/libsz.a $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         $cmd = "cp $zldir/lib/libz.a $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
      } elsif ($dir eq "freedom" || $dir eq "loyalty") {
         $cmd = "cp $szdir/lib/libsz.so.2 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         $cmd = "cp $zldir/lib/libz.so.5 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         
         chdir "$indirectory/$dir/lib" or die "Couldn't change directory to $indirectory/$dir/lib, $!";
         $cmd = "ln -s libsz.so.2 libsz.so";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libz.so.5 libz.so";
         $output = `$cmd`;
         print $output;

         chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";
      } elsif ($dir eq "quail" || $dir eq "kite" || $dir eq "osx1010test") {
         #we've been using the static libraries for the macs - built with -fPIC
#         $cmd = "cp $szdir/lib/libsz.a $indirectory/$dir/lib";
#         $output = `$cmd`;
#         print $output;
#         $cmd = "cp $zldir/lib/libz.a $indirectory/$dir/lib";
#         $output = `$cmd`;
#         print $output;
         $cmd = "cp $szdir/lib/libsz.2.0.0.dylib $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         $cmd = "cp $zldir/lib/libz.1.2.5.dylib $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;

         chdir "$indirectory/$dir/lib" or die "Couldn't change directory to $indirectory/$dir/lib, $!";
         $cmd = "ln -s libsz.2.0.0.dylib libsz.2.dylib";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libsz.2.0.0.dylib libsz.dylib";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libz.1.2.5.dylib libz.1.dylib";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libz.1.2.5.dylib libz.dylib";
         $output = `$cmd`;
         print $output;

         chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";

      } elsif ($dir eq "emu64") {
         $cmd = "cp $szdir/lib/libsz.so.2.0.0 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         $cmd = "cp $zldir/lib/libz.a $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         
         chdir "$indirectory/$dir/lib" or die "Couldn't change directory to $indirectory/$dir/lib, $!";
         $cmd = "ln -s libsz.so.2.0.0 libsz.so.2";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libsz.so.2.0.0 libsz.so";
         $output = `$cmd`;
         print $output;

         chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";

      } elsif ($dir eq "platypus32" || $dir eq "moohan32") {
         $cmd = "cp $szdir/lib/libsz.so.2.0.0 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;

        # $cmd = "cp $zldir/lib/libz.a $indirectory/$dir/lib";
        # $output = `$cmd`;
        # print $output;

         chdir "$indirectory/$dir/lib" or die "Couldn't change directory to $indirectory/$dir/lib, $!";
         $cmd = "ln -s libsz.so.2.0.0 libsz.so.2";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libsz.so.2.0.0 libsz.so";
         $output = `$cmd`;
         print $output;

        # $cmd = "ln -s libz.so.1.2.8 libz.so.1";
        # $output = `$cmd`;
        # print $output;

        # $cmd = "ln -s libz.so.1.2.8 libz.so";
        # $output = `$cmd`;
        # print $output;

         chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";

      } else {
         $cmd = "cp $szdir/lib/libsz.so.2.0.0 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;

         $cmd = "cp $zldir/lib/libz.so.1.2.5 $indirectory/$dir/lib";
         $output = `$cmd`;
         print $output;
         
         chdir "$indirectory/$dir/lib" or die "Couldn't change directory to $indirectory/$dir/lib, $!";
         $cmd = "ln -s libsz.so.2.0.0 libsz.so.2";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libsz.so.2.0.0 libsz.so";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libz.so.1.2.5 libz.so.1";
         $output = `$cmd`;
         print $output;

         $cmd = "ln -s libz.so.1.2.5 libz.so";
         $output = `$cmd`;
         print $output;

         chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";
      }
   }
}

sub makeTarFile { 
   my $directoryname = shift;
   my $origdirname = shift;
   my $tarfilename = shift;
   my $tardirname = shift;
   $cmd = "mv $indirectory/$origdirname $indirectory/$tardirname";
   $output = `$cmd`;
   print $output;
   if (-d $indirectory."/".$tardirname."/doc/hdf5/examples") {
      $cmd = "mv $indirectory/$tardirname/doc/hdf5/examples $indirectory/$tardirname/examples";
      $output = `$cmd`;
      print $output;
      $cmd = "rm -rf $indirectory/$tardirname/doc";
      $output = `$cmd`;
      print $output;
   }
   $cmd = "tar zcvf $outdirectory/$directoryname/$tarfilename -C $indirectory $tardirname";
   print "Need to run $cmd.\n";
   $output = `$cmd`;
   sleep 10;
   print "Create $tarfilename:  $output\n";
   $cmd = "mv $indirectory/$tardirname $indirectory/$origdirname";
   $output = `$cmd`;
   print $output;
}

foreach $key (keys %destsubdir) {
   print "Process ".$key."\n\n";
   #skip unless there's a directory by the name of $key or $key-static
   next unless -d $indirectory.'/'.$key || -d $indirectory.'/'.$key."-static";
   my $version;
   # This assumes a static directory.  Probably the others should be checked if this
   # doesn't exist.
   $cmd = "grep \"HDF5 Version\" $indirectory/$key/lib/libhdf5.settings";
   $_ = `$cmd`;
   print $_, "\n";
   s/HDF5 Version://;
   s/^\s+//;
   chomp;
   $version = $_;
   #my $directoryname = substr $destsubdir{$key}, 0, rindex($destsubdir{$key}, '-');
   my $directoryname = $destsubdir{$key};
   mkdir $outdirectory."/".$directoryname, 0755 unless -d $outdirectory."/".$directoryname;
   my $staticdir = $key."-static";
   print $indirectory."/$key tarfile will be put in " . $outdirectory."/".$directoryname."\n";
   if (-e $outdirectory."/".$destsubdir{$key}."/README") {
      print $outdirectory."/".$destsubdir{$key}."/README" . " has already been created.\n";
   }
   else {
      print "Make the Outer README file:  ";
      $cmd = "perl ./makeOuterREADME.pl $indirectory/$key $outdirectory $directoryname";
      print $cmd, "\n";
      my $output = `$cmd`;
      print $output;
   }
   my $file = "";
   my @dirnames = "";
   opendir(DIR, $indirectory) or die "can't open .: $!";
   while (defined($file = readdir(DIR))) {
      next unless ($file eq $key || $file eq "$key-static") && -d $indirectory."/".$file;
      push @dirnames, $file;
   }
   foreach my $dir (@dirnames) {
      next if $dir eq "";
      print "Make the Inner README files.\n";
      $cmd = "perl ./makeInternalREADME.pl $indirectory/$dir";
      print $cmd, "\n";
      $output = `$cmd`;
      print $output;
      print "Add the zlib and szip files for $dir.\n";
      &addzandszlibs($dir, $indirectory);
      my $currentdir = getcwd();
     
      print "Remove all lib*.la files from $dir/lib*.\n";
      $cmd = "rm $indirectory/$dir/lib*/lib*.la";
      print $cmd, "\n";
      $output = `$cmd`;
      print $output;

      chdir "$indirectory/$dir/bin" or die "Couldn't change directory to $indirectory/$dir/bin, $!";

      $cmd = "$scriptdirname/h5rmflags -force";
      $output = `$cmd`;
      print $output;
      chdir $currentdir or die "Couldn't change directory back to $currentdir, $!";
      print "Tar up the files into the output directory.\n";
      if (-d $indirectory."/".$dir) {
         if ($dir =~ /static/) {
            $tarfilename = "hdf5-$version-$destsubdir{$key}-static.tar.gz";
            $tardirname = "hdf5-$version-$destsubdir{$key}-static";
         } else {
            $tarfilename = "hdf5-$version-$destsubdir{$key}-shared.tar.gz";
            $tardirname = "hdf5-$version-$destsubdir{$key}-shared";
         }            
         &makeTarFile($directoryname, $dir, $tarfilename, $tardirname);
      }
   } 
      # If this copy is done after h5rmflags is run on all the directories the compile scripts
      # in the utilities directory will already have the paths removed. 
   if (-d $indirectory."/".$staticdir) {
      $cmd = "cp -prv $indirectory/$staticdir/bin $outdirectory/$directoryname/utilities";
      $output = `$cmd`;
      print $output;
   }
}

