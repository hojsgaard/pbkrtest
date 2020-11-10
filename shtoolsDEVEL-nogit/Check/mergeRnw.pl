#!/usr/bin/perl

# Reads a .Rnw file say foo.Rnw which may have several .Rnw files included in it and  
# and merges it all into one file foo-one.Rnw which can subsequently be processed by
# Sweave and Stangle 

$numArgs = $#ARGV + 1;
my $mainfilename = $ARGV[0];
my $mainfilehead = $mainfilename;
$mainfilehead =~ s/\.Rnw//g;
my $outfilename = "$mainfilehead.ONE";

print("Input  (main file) : $mainfilehead \n");
print("Output (main file) : $outfilename \n");

# my $mainfilename = "Main.Rnw";

open(OUT, "> $outfilename");
print OUT "%%\n%% AUTOMATICALLY GENERATED FILE - DO NOT EDIT MANUALLY\n";
print OUT "%% File origin : $mainfilename \n%%\n";
close(OUT);

open(OUT, ">> $outfilename");

if ( !open( MAIN, $mainfilename ) )
{
    die( "error reading file seq1.txt: $!" );
}

while( !eof( MAIN ) ) {
    my $line = <MAIN>;
    if ( ($line =~ /^\\input\{/) || ($line =~ /^\\include\{/)  ){
	print ("Line contains input or include...\n");
	$line =~ s/.+\{//g;
	#print( "$line \n"); 
	$line =~ s/\}//g;
	#print( "$line \n");
	readSubFile($line);
    } else {
	print OUT "$line" ;
    }
}

sub readSubFile 
{    
    my $infilename = shift;
    
    if ( !open( IN, $infilename ) )
    {
	die( "error reading file seq1.txt: $!" );
    }
    
    print OUT "\n%% BEGIN: Content of file $infilename\n";
    
    while( !eof( IN ) ) {
	my $line = <IN>;
	print( $line );
	print OUT "$line" ;
    }
    
    close( IN );
    
    print OUT "\n%% END: Content of file $infilename\n";
    return $infilename;
}


