#!/usr/bin/perl

use strict;

my $index = 0;
my $num_lines = 0;

open FILE, ">/void/endo/rna.$index";
while (<>) {
    if (/^\+/ || /^\*/) {
	if ($num_lines > 0) {
	    close FILE;
	    ++$index;
	    open FILE, ">/void/endo/rna.$index";
	    $num_lines = 0;
	}
    } else {
	++$num_lines;
	print FILE;
    }
}

print "$index\n";
