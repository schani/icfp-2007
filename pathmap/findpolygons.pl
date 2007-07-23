#!/usr/bin/perl

use strict;

my $dna = <STDIN>;

sub bin2dec {
    return unpack("N", pack("B32", substr("0" x 32 . shift, -32)));
}


while ($dna =~ /((C|F){11}IC){16,}/g) {
    my $poly = $&;
    my $addr = length $`;
    my $size = length $poly;
    my $end = $addr + $size;
    my $index = 0;
    my $xpos = 0;
    my $ypos = 0;
    my $dx = 0;
    my $dy = 0;

    $poly =~ s/IC/P/g;
    $poly =~ tr/CF/IC/;
    
    print "<path id=\"Poly @ $addr\t+ $size\t= $end\"\n";
    print "\tfill=\"none\" stroke=\"black\" stroke-width=\"1\"\n";
    print "\td=\"M ";
    
   
    my $num = 0;
    while ($poly =~ /([IC]*)P/g) {
       my $point = reverse $1;
       
       $point =~ tr/IC/01/;
       my $value = bin2dec($point);
       
       if ($value > 1023) {
           $value = $value - 2048;
       }
       
       if ($index == 0) { 
           $num = $value; 
       } elsif ($index < $num*2-1) {
           if ($index > 2) {
	       if ($index % 2) {
	           $xpos += $dx;
 	           print "$xpos.00,";
	       } else {
 	           $ypos += $dy;
	           print "$ypos.00 ";
	       }
	   }
           if ($index % 2) {
 	       $dx = $value;
	   } else {
	       $dy = $value;
	   }
 	   if ($index == 2) {
 	       print "$dx.00,$dy.00\n\tC ";
 	   } elsif (($index % 4) == 2) {
 	       print "$xpos.00,$ypos.00\n\t";
 	   }
       }
       
       # print "$index: $value.00\n";
       $index++;     
    }
    print "\" />\n";
}
