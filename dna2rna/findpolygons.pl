#!/usr/bin/perl

use strict;

my $dna = <STDIN>;

while ($dna =~ /((C|F){11}IC){20,}/g) {
    my $poly = $&;

    $poly =~ s/IC/P/g;
    $poly =~ tr/CF/IC/;
    print "found match at ", length($`), ": $poly\n";
}
