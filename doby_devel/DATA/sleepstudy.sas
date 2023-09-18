
%macro importcsv (data, infile);
PROC IMPORT OUT= &data
     DATAFILE= &infile  DBMS=CSV REPLACE; GETNAMES=YES; DATAROW=2; RUN;
%mend;

%importcsv(sleep, "C:\Dokumenter\Stat\Rdevel\doByDEVEL\sleepstudy.csv")

proc mixed data=sleep method=reml;
  class daysf subject;
  model reaction = daysf / noint solution soloutionr covb;
  random int/ subject=subject;
  lsmeans daysf;
run;


proc mixed data=sleep;
  class daysf subject;
  model reaction = daysf /noint solutionf soloutionr;
  lsmeans daysf;
run;
