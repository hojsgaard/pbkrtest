#!/usr/bin/perl

##
## foo.Rnw complieres med Sweave til foo.tex
## Det antages nedenfor, at alle figurer er gemt som pdf.
## Kør sweave2html foo. 
## 1) Først laves foo-tth.tex
## 2) Derefter køres tth på foo-tth.tex. Det laver foo-tth.html
## 3) Derefter laves foo-tth.html om til foo.html
##
## foo.html sammen med folderen med figurerne kan nu offentliggøres.
##

$numArgs = $#ARGV + 1;
my $inFile = $ARGV[0];

open(IN, $inFile.".tex");
my $outFile = $inFile."-tth.tex";

print "Infile:   $inFile \n";
print "Outfile:  $outFile \n";
print "HTMLfile: $inFile.html \n";

open(OUT, ">".$outFile);
while( !eof( IN ) )
{
    my $line = <IN>;
    $_ = $line;
    if (/Schunk|\\SweaveOpts|\\setkeys|\\RecustomVerbatimEnvironment/){
	$_ = "";
    }
    if (/Sinput|Soutput/){
	s/Sinput|Soutput/verbatim/;
    }
    if (/includegraphics/){
	my @fields = split /\{|\}/, $_;
	my $figFile = @fields[1];
	my $str = "convert $figFile.pdf gif:$figFile.gif";
	print "$str \n";
	system "$str";
	#my $figFile2 = $figFile;
	#$figFile2 =~ s/\//\\/; ## Erstat / med \
	#print "$figFile2 \n";
	#my $str = "imcon $figFile2.gif -g --o $figFile2.gif --y400 --k";
	#print "$str \n";
	#system "$str";
	s/\}/\.gif\}/;
    }
    print OUT $_;
}
close( IN );
close( OUT );

system "cp $inFile.toc $inFile-tth.toc";
system "tth -e2 < $outFile > $inFile.html -L$inFile-tth ";
$str = "move $inFile-tth.html $inFile.html";
system "$str";


