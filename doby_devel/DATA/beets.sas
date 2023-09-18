
%macro importcsv (data, infile);
PROC IMPORT OUT= &data
     DATAFILE= &infile  DBMS=CSV REPLACE; GETNAMES=YES; DATAROW=2; RUN;
%mend;

%importcsv(beets, "C:\Documents\stat\Rdevel\doByDEVEL\DATA\beets-reduced.csv")

%importcsv(beets, "C:\Documents\stat\Rdevel\doByDEVEL\DATA\beets.csv")

%importcsv(warpbreaks, "C:\Documents\stat\Rdevel\doByDEVEL\DATA\warpbreaks.csv")

proc mixed data=beets;
	class harvest sow block;
	model yield = block sow harvest;
	random block*harvest;
	lsmeans sow;
run;

proc mixed data=beets;
	class harvest sow block;
	model yield = block sow harvest /ddfm=kenward;
	random block*harvest;
	lsmeans sow;
run;


proc mixed data=beets;
	class harvest sow block;
	model yield = block sow harvest;
	lsmeans sow;
run;

proc mixed data=warpbreaks;
	class tension wool;
	model breaks = tension / ddfm=kenward;
	random wool;
	lsmeans tension;
	run;

