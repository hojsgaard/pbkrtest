%macro importexcel (data, infile);
PROC IMPORT OUT= &data 
            DATAFILE= &infile
            DBMS=EXCEL5 REPLACE;     GETNAMES=YES;
RUN;
%mend;


%macro importcsv (data, infile);
PROC IMPORT OUT= &data 
            DATAFILE= &infile 
            DBMS=CSV REPLACE;
     GETNAMES=YES;     DATAROW=2; 
RUN;
%mend;

%importcsv(dietox, "dietox.csv")
proc print data=dietox; run;

proc mixed data=dietox;
  class pig Cu;
  model weight = time|time|time Cu|time|time;
  lsmeans Cu;
run;
