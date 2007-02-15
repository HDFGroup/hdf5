#!/usr/bin/perl
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

#
#    Generates an ASCII and Excel-importable file of tables representing
#    the output of running the "pio_perf" command. The name of the input
#    file is important. The name should reflect the command-line options
#    used in the performance test. It needs to be of the form:
#
#        f#[GMK].i#.d#.X#[GMK].x#[GMK]..*
#
#    For example:
#
#        PIO_output_f1G.i2.d1.X2M.x128K.frost
#
#    for a 1GB sized file ran for 2 iterations with 1 dataset from xfer
#    buffer size of 128KB to 2MB on the frost machine.
#
#    The output file will have the same name as the input, but will append
#    ".ascii" for the ASCII file and ".excel" for the Excel-importable
#    file.
#
#    The data structure used in this program looks like:
#
#        %results = {
#            num_proc => (
#                %xfer_size => (
#                    %posix = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    },
#                    %mpio = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    },
#                    %phdf = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    }
#                )
#            )
#        }

if ($#ARGV != 0) {
	print "gen_report.pl [FILE]\n";
	exit 1;
}

my ($ascii_output, $excel_output);

foreach my $arg (@ARGV) {
	if ($arg !~ /^-/) {
		$arg =~ /f([0-9]+.)\.i([0-9]+)\.d([0-9]+)\.X([0-9]+.)\.x([0-9]+.)\.(.*)/;

		my $output = "result_f" . $1 . ".X" . $4 . ".x" . $5 . "." . $6;

		$ascii_output = $output . ".ascii";
		$excel_output = $output . ".excel";

		open(INPUT, "<$arg") or die "error: cannot open file $arg: $!\n";
		open(ASCII_OUTPUT, ">$ascii_output") or
				die "error: cannot open file $ascii_output: $!\n";
		open(EXCEL_OUTPUT, ">$excel_output") or
				die "error: cannot open file $excel_output: $!\n";
	} else {
		die "error: unrecognized option: $arg\n";
	}
}

my %results;
my $num_procs = 0;
my ($xfer_size, $avg_type, $type);

my $posix = 0, $mpio = 1, $phdf5 = 2;

##"==== End of Parameters ===="

while (<INPUT>) {
	if (/Number of processors = ([0-9]+)/) {
		$num_procs = $1;
	}

	if (/Transfer Buffer Size: ([0-9]+)/) {
		$xfer_size = $1;
	}

	$type = $posix if /POSIX/;
	$type = $mpio if /MPIO/;
	$type = $phdf5 if /PHDF5/;

	if (/Write Open/) {
		$avg_type = "write-close";
	} elsif (/Write/) {
		$avg_type = "write-only";
	} elsif (/Read Open/) {
		$avg_type = "read-close";
	} elsif (/Read/) {
		$avg_type = "read-only";
	}

	if (/Maximum Throughput: ( ?[0-9]+\.[0-9]{2}) MB\/s/) {
		$results{$num_procs}{$xfer_size}[$type]{$avg_type} = $1;
	}
}

sub create_excel_output_header {
	my $output_header;
	my $kb = 1024;
	my $mb = $kb * $kb;

	foreach my $key (sort { $a <=> $b } keys(%{$results{$num_procs}})) {
		if ($key < $mb) {
			$key /= $kb;
			$output_header .= "\t". $key . "K";
		} else {
			$key /= $mb;
			$output_header .= "\t". $key . "M";
		}
	}

	$output_header;
}

sub create_excel_output_string {
	my ($t) = @_;
	my $output_content = "";

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		$output_content .= "\n$procs Procs";
		$output_content .= "\n" . create_excel_output_header;
		$output_content .= "\n  POSIX";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[0]{$t}";
		}

		$output_content .= "\n  MPIO";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[1]{$t}";
		}

		$output_content .= "\n  PHDF5";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[2]{$t}";
		}

		$output_content .= "\n";
	}

	$output_content;
}

sub is_defined {
	my ($t) = @_;
	my $def = 1;

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}
	}

	$def;
}

sub write_excel_file {
	print EXCEL_OUTPUT "\nWrite-Only\n";
	print EXCEL_OUTPUT create_excel_output_string("write-only");
	print EXCEL_OUTPUT "\nWrite-Close\n";
	print EXCEL_OUTPUT create_excel_output_string("write-close");

	if (is_defined("read-only")) {
		print EXCEL_OUTPUT "\nRead-Only\n";
		print EXCEL_OUTPUT create_excel_output_string("read-only");
		print EXCEL_OUTPUT "\nRead-Close\n";
		print EXCEL_OUTPUT create_excel_output_string("read-close");
	}
}

sub create_ascii_output_header {
	my $output_header = " " x 12 . "|";
	my $kb = 1024;
	my $mb = $kb * $kb;

	foreach my $key (sort { $a <=> $b } keys(%{$results{$num_procs}})) {
		if ($key < $mb) {
			$key /= $kb;
			$output_header = sprintf("$output_header   %-4s   |", $key .  "K");
		} else {
			$key /= $mb;
			$output_header = sprintf("$output_header   %-4s   |", $key .  "M");
		}
	}

	$output_header;
}

sub create_ascii_output_string {
	my ($t) = @_;
	my $output_content = "";
	my $output_header = create_ascii_output_header;

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		$output_content .= "\n$procs Procs";
		$output_content .= "\n$output_header\n";
		$output_content .= "-" x length($output_header);
		$output_content .= "\n     POSIX  |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[0]{$t});
		}

		$output_content .= "\n    ";
		$output_content .= "-" x (length($output_header) - 4);
		$output_content .= "\n     MPI/IO |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[1]{$t});
		}

		$output_content .= "\n    ";
		$output_content .= "-" x (length($output_header) - 4);
		$output_content .= "\n     PHDF5  |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[2]{$t});
		}

		$output_content .= "\n";
		$output_content .= "=" x length($output_header);
		$output_content .= "\n";
	}

	$output_content;
}

sub write_ascii_file {
	print ASCII_OUTPUT "\nWrite-Only";
	print ASCII_OUTPUT "\n----------\n";
	print ASCII_OUTPUT create_ascii_output_string("write-only");
	print ASCII_OUTPUT "\n\nWrite-Close";
	print ASCII_OUTPUT "\n-----------\n";
	print ASCII_OUTPUT create_ascii_output_string("write-close");

	if (is_defined("read-only")) {
		print ASCII_OUTPUT "\n\nRead-Only";
		print ASCII_OUTPUT "\n---------\n";
		print ASCII_OUTPUT create_ascii_output_string("read-only");
		print ASCII_OUTPUT "\n\nRead-Close";
		print ASCII_OUTPUT "\n----------\n";
		print ASCII_OUTPUT create_ascii_output_string("read-close");
	}
}

write_excel_file;
write_ascii_file;
