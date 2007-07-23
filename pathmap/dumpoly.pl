#!/usr/bin/perl

use strict;

my $dna = <STDIN>;

sub bin2dec {
    return unpack("N", pack("B32", substr("0" x 32 . shift, -32)));
}


while ($dna =~ /(IIIPIIPICP.*?)((C|F){11}IC){16,}/g) {
    my $color = $1;
    my $poly = $&;
    my $addr = length $`;
    my $clen = length $color;
    my $plen = length $poly;
    my $paddr = $addr + length $color;
    
    print "addr: $addr/$paddr\n";
    print "lens: $clen/$plen\n";
    print "cols: $color\n";
   
}
