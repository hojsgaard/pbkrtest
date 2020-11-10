%macro importcsv (data, infile);
PROC IMPORT OUT= &data 
            DATAFILE= &infile 
            DBMS=CSV REPLACE;
     GETNAMES=YES;     DATAROW=2; 
RUN;
%mend;

%macro exportcsv (data, outfile);
PROC EXPORT DATA= &data 
            OUTFILE= &outfile
            DBMS=CSV REPLACE;
RUN;
%mend;

/*---Data Set 2.2(a)---*/

%importcsv(dietox, "E:\Stat\Rdevel\SHtoolsDEVEL\dietox.csv");











data a;
   input block cult$ inoc$ drywt;
   datalines;
1 a con 27.4
1 a dea 29.7
1 a liv 34.5
1 b con 29.4
1 b dea 32.5
1 b liv 34.4
2 a con 28.9
2 a dea 28.7
2 a liv 33.4
2 b con 28.7
2 b dea 32.4
2 b liv 36.4
3 a con 28.6
3 a dea 29.7
3 a liv 32.9
3 b con 27.2
3 b dea 29.1
3 b liv 32.6
4 a con 26.7
4 a dea 28.9
4 a liv 31.8
4 b con 26.8
4 b dea 28.6
4 b liv 30.7
;
run;
%exportcsv(a, "E:\Konsultationer\HEF-konsulent-mixedRecap+R\Inoculation.csv");


data weights;
   input subj program$ s1 s2 s3 s4 s5 s6 s7;
   datalines;
 1      CONT      85    85    86    85    87    86    87
 2      CONT      80    79    79    78    78    79    78
 3      CONT      78    77    77    77    76    76    77
 4      CONT      84    84    85    84    83    84    85
 5      CONT      80    81    80    80    79    79    80
 6      CONT      76    78    77    78    78    77    74
 7      CONT      79    79    80    79    80    79    81
 8      CONT      76    76    76    75    75    74    74
 9      CONT      77    78    78    80    80    81    80
10      CONT      79    79    79    79    77    78    79
11      CONT      81    81    80    80    80    81    82
12      CONT      77    76    77    78    77    77    77
13      CONT      82    83    83    83    84    83    83
14      CONT      84    84    83    82    81    79    78
15      CONT      79    81    81    82    82    82    80
16      CONT      79    79    78    77    77    78    78
17      CONT      83    82    83    85    84    83    82
18      CONT      78    78    79    79    78    77    77
19      CONT      80    80    79    79    80    80    80
20      CONT      78    79    80    81    80    79    80
 1      RI        79    79    79    80    80    78    80
 2      RI        83    83    85    85    86    87    87
 3      RI        81    83    82    82    83    83    82
 4      RI        81    81    81    82    82    83    81
 5      RI        80    81    82    82    82    84    86
 6      RI        76    76    76    76    76    76    75
 7      RI        81    84    83    83    85    85    85
 8      RI        77    78    79    79    81    82    81
 9      RI        84    85    87    89    88    85    86
10      RI        74    75    78    78    79    78    78
11      RI        76    77    77    77    77    76    76
12      RI        84    84    86    85    86    86    86
13      RI        79    80    79    80    80    82    82
14      RI        78    78    77    76    75    75    76
15      RI        78    80    77    77    75    75    75
16      RI        84    85    85    85    85    83    82
 1      WI        84    85    84    83    83    83    84
 2      WI        74    75    75    76    75    76    76
 3      WI        83    84    82    81    83    83    82
 4      WI        86    87    87    87    87    87    86
 5      WI        82    83    84    85    84    85    86
 6      WI        79    80    79    79    80    79    80
 7      WI        79    79    79    81    81    83    83
 8      WI        87    89    91    90    91    92    92
 9      WI        81    81    81    82    82    83    83
10      WI        82    82    82    84    86    85    87
11      WI        79    79    80    81    81    81    81
12      WI        79    80    81    82    83    82    82
13      WI        83    84    84    84    84    83    83
14      WI        81    81    82    84    83    82    85
15      WI        78    78    79    79    78    79    79
16      WI        83    82    82    84    84    83    84
17      WI        80    79    79    81    80    80    80
18      WI        80    82    82    82    81    81    81
19      WI        85    86    87    86    86    86    86
20      WI        77    78    80    81    82    82    82
21      WI        80    81    80    81    81    82    83
;
               
/*---Data Set 3.2(b)---*/

data weight2; 
   set weights;
   time=1; strength=s1; output;
   time=2; strength=s2; output;
   time=3; strength=s3; output;
   time=4; strength=s4; output;
   time=5; strength=s5; output;
   time=6; strength=s6; output;
   time=7; strength=s7; output;
   keep subj program time strength;
run;
    
proc sort data=weight2; 
   by program time;
run;

%importcsv(dietox, "E:\Konsultationer\HEF-konsulent-mixedRecap+R\dietox.csv");
