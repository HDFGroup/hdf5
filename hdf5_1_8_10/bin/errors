#!/usr/local/bin/perl -w
require 5.003;
use Text::Tabs;

# NOTE: THE FORMAT OF HRETURN_ERROR AND HGOTO_ERROR MACROS HAS
# CHANGED. THIS SCRIPT NO LONGER WORKS! --rpm

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
# Purpose: This script will read standard input which should be a
#	   function prologue followed by a C function and will emit
#	   on standard output the same source code with the function
#	   prologue containing documentation for the various errors
#	   that occur in the function.
#
#	   Errors are raised by calling HGOTO_ERROR() or
#	   HRETURN_ERROR(). The reason for the error message is a
#	   comment which appears immediately after the error macro
#	   call and is contained entirely on one line:
#
#		HRETURN_ERROR (...); /*entry not found*/
#
#	   If such a comment doesn't exist, then the previous comment
#	   is used, subject to the constraint that raising an error
#	   clears the previous comment.
#
#	         /* Entry not found */
#		 HGOTO_ERROR (...);
#
# 	   Emacs users can use this script interactively with the
#	   c-mark-function and shell-command-on-region functions which
#	   are normally bound to M-C-h and M-|.


# Split STDIN into the prolog and the function body.  Preserve leading
# white space.
$_ = join "", <STDIN>;
my ($head, $prolog, $body) = (/^(\s*)(\/\*(.*?)\*\/)?(.*)/s)[0,2,3];
$prolog = "" unless $prolog;

# Find each error and the comment that goes with it.
for ($_=$body,$comment=""; /\/\*|H(RETURN|GOTO)_ERROR/s;) {
   $_ = $&.$';

   if (/^H(RETURN|GOTO)_ERROR\s*\(\s*H5E_(\w+)\s*,\s*H5E_(\w+)\s*,/s) {
      ($major, $minor, $_) = ($2, $3, $');
      $comment=$1 if /^.*?\)\s*;\s*\/\*\s*(.*?)\s*\*\//;
      $comment =~ s/^\s*\*+\s*/ /mg; # leading asterisks.
      $comment =~ s/^\s+//s;         # leading white space.
      $comment =~ s/\s+$//s;         # trailing white space.
      $comment =~ s/(\w)$/$1./s;     # punctuation.
      $comment ||= "***NO COMMENT***";
      $errors{"$major\000$minor\000\u$comment"} = 1;
      $comment = "";

   } else {
      ($comment) = /^\/\*\s*(.*?)\s*\*\//s;
      $_ = $';
   }
}


# Format an error so it isn't too wide.
sub fmt_error ($) {
   local ($_) = @_;

   my ($prefix,$space,$err) = /^((.*?)([A-Z_0-9]+\s+[A-Z_0-9]+\s+))/;
   $_ = $';
   tr/\n /  /s;
   my $w = 70 - length expand $prefix;
   s/(.{$w}\S+)\s+(\S)/$1."\n".$space.' 'x(length $err).$2/eg;
   return $prefix . $_."\n";
}



# Sort the errors by major, then minor, then comment.  Duplicate
# triplets have already been removed.
sub by_triplet {
   my ($a_maj, $a_min, $a_com) = split /\000/, $a;
   my ($b_maj, $b_min, $b_com) = split /\000/, $b;
   $a_maj cmp $b_maj || $a_min cmp $b_min || $a_com cmp $b_com;
}
@errors = map {sprintf "%-9s %-13s %s\n", split /\000/}
          sort by_triplet keys %errors;



# Add the list of errors to the prologue depending on the type of
# prolog.
if (($front, $back) = $prolog=~/^(.*?Errors:\s*?(?=\n)).*?\n\s*\*\s*\n(.*)/s) {
   #| * Errors:							      |#
   #| *		__list_of_error_messages__ (zero or more lines)	      |#
   #| *								      |#
   print $head, "/*", $front, "\n";
   map {print fmt_error " *\t\t".$_} @errors;
   print " *\n", $back, "*/", $body;

} elsif (($front,$back) = $prolog =~
	 /(.*?\n\s*ERRORS:?\s*?(?=\n)).*?\n\s*\n(.*)/s) {
   #| ERRORS							      |#
   #|		__list_of_error_messages__ (zero or more lines)	      |#
   #|								      |#
   print $head, "/*", $front, "\n";
   map {print fmt_error "    ".$_} @errors;
   print "\n", $back, "*/", $body;

} elsif ($prolog eq "") {
   # No prolog present.
   print $head;
   print "\n/*", "-"x73, "\n * Function:\t\n *\n * Purpose:\t\n *\n";
   print " * Errors:\n";
   map {print fmt_error " *\t\t".$_} @errors;
   print " *\n * Return:\tSuccess:\t\n *\n *\t\tFailure:\t\n *\n";
   print " * Programmer:\t\n *\n * Modifications:\n *\n *", '-'x73, "\n";
   print " */\n", $body;

} else {
   # Prolog format not recognized.
   print $head, "/*", $prolog, "*/\n\n";
   print "/*\n * Errors returned by this function...\n";
   map {print fmt_error " *\t".$_} @errors;
   print " */\n", $body;
}


