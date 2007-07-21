#!/usr/bin/perl

my $line = <STDIN>;

my %patterns = ( "black" => "PIPIIIC",
		 "red" => "PIPIIIP",
		 "green" => "PIPIICC",
		 "yellow" => "PIPIICF",
		 "blue" => "PIPIICP",
		 "magenta" => "PIPIIFC",
		 "cyan" => "PIPIIFF",
		 "white" => "PIPIIPC",
		 "transparent" => "PIPIIPF",
		 "opaque" => "PIPIIPP",
		 "clearbucket" => "PIIPICP",
		 "move" => "PIIIIIP",

		 "mark" => "PCCIFFP",
		 "fill" => "PIIPIIP",
		 
		 "ccw" => "PCCCCCP",
		 "cw" => "PFFFFFP",
		 "line" => "PFFICCP",
		 "add_bitmap" => "PCCPFFP",
		 "compose" => "PFFPCCP",
		 "clip" => "PFFICCF" );

sub const {
    my $pat = shift;

    $pat =~ s/IC/p/;
    $pat =~ tr/CFPp/ICFP/;

    return $pat;
}

foreach $name (keys %patterns) {
    my $pat = $patterns{$name};

  again:
    print "$name ($pat): ";
    if ($line =~ /$pat/) {
	print "match\n";
    }
    if (!($pat =~ /I[^C]/)) {
	$pat = const($pat);
	goto again;
    }
}
