#!/usr/bin/perl

use strict;

my $line = <STDIN>;

$line =~ s/PIPIIPP/PIPIIPF/g;

print $line;
