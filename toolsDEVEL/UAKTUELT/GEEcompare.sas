title "fm0";
proc genmod data=dietox;
  class cu pig;
  model weight = time cu cu * time / type1 type3 d=poisson;
  repeated subject=pig;
  lsmeans cu;
run;



























/*---produces Output 2.10 on pages 58-59---*/
options nocenter;
proc mixed data=a;
   class block cult inoc;
   model drywt = cult inoc cult*inoc / ddfm=satterth;
   random block block*cult;
run;

proc mixed data=a;
   class block cult inoc;
   model drywt = cult inoc cult*inoc / ddfm=satterth;
   random int cult /subject=block;
   lsmeans cult cult*inoc;
run;



*** WEIGHTLIFTING data for random regression models ***;


               
               
/*---produces Output 3.1 on pages 91-92---*/

proc mixed data=weight2;
   class program subj time;
   model strength = program time program*time;
   random int / subject=subj(program);
run;


/*---produces Output 3.2 on pages 94-96---*/

proc mixed data=weight2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=cs sub=subj(program) r rcorr;
run;


/*---produces Output 3.3 on pages 97-99---*/

proc mixed data=weight2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=ar(1) sub=subj(program) r rcorr;
run;


/*---produces Output 3.4 on pages 99-101---*/

proc mixed data=weight2;
   class program subj time;
   model strength = program time program*time;
   repeated / type=un sub=subj(program) r rcorr;
run;


/*---produces Output 3.5 on page 103---*/

proc mixed data=weight2;
   class program subj;
   model strength = program time time*program 
   time*time time*time*program / htype=1;
   repeated / type=ar(1) sub=subj(program);
run;


/*---produces Output 3.6 on page 104---*/

proc mixed data=weight2;
   class program subj;
   model strength = program time*program time*time*program / noint 
      s htype=1;
   repeated / type=ar(1) sub=subj(program);
run;


************************************************;
***
*** DIETOX data for random regression models ***;
***
************************************************;

proc print data=dietox; run;

title "fm0";
proc mixed data=dietox noinfo noclprint;
  class cu pig;
  model weight = time cu cu * time / solution htype=1;
run;

title "fm1";
proc mixed data=dietox noinfo noclprint;
  class cu pig;
  model weight = time cu cu * time / solution htype=1;
  random int / subject=cu*pig;
run;

title "fm2";
proc mixed data=dietox noinfo noclprint;
  class cu pig;
  model weight = time cu cu * time / solution htype=1;
  random int time/ subject=pig;
run;

title "fm3";
data dietox; set dietox; timec = time;
proc mixed data=dietox noinfo noclprint;
  class cu pig timec;
  model weight = time cu cu * time / solution htype=1;
  random int time/ subject=cu*pig;
  repeated timec / subject=cu*pig;
run;


title "fm4";
proc mixed data=dietox noinfo noclprint;
  class cu pig timec;
  model weight = time time*time cu cu * time cu*time*time/ solution htype=1;
  random int time/ subject=pig;
run;

proc mixed data=dietox noinfo noclprint;
  class cu pig timec;
  model weight = time time*time time*time*time cu 
       cu*time cu*time*time cu*time*time*time / 
       solution htype=1;
  random int time/ subject=pig;
  repeated timec / subject=pig;
run;


  repeated timec / subject=pig;



************************************************;
***
*** WHEAT data for random regression models ***;
***
************************************************;

data wheat;
   input id variety yield moist;
   datalines;
 1       1         41       10
 2       1         69       57
 3       1         53       32
 4       1         66       52
 5       1         64       47
 6       1         64       48
 7       2         49       30
 8       2         44       21
 9       2         44       20
10       2         46       26
11       2         57       44
12       2         42       19
13       3         69       50
14       3         62       40
15       3         50       23
16       3         76       58
17       3         48       21
18       3         55       30
19       4         48       22
20       4         60       40
21       4         45       17
22       4         47       21
23       4         62       44
24       4         43       13
25       5         65       49
26       5         63       44
27       5         71       57
28       5         68       51
29       5         52       27
30       5         68       52
31       6         76       55
32       6         46       11
33       6         45       11
34       6         67       43
35       6         65       38
36       6         79       60
37       7         35       17
38       7         37       20
39       7         30       11
40       7         30       10
41       7         57       48
42       7         49       36
43       8         75       57
44       8         64       41
45       8         46       15
46       8         54       28
47       8         52       23
48       8         52       23
49       9         51       26
50       9         63       44
51       9         42       13
52       9         61       40
53       9         67       48
54       9         69       53
55      10         60       37
56      10         73       58
57      10         66       44
58      10         71       53
59      10         67       48
60      10         74       59
;


/*---produces Outputs 7.1 and 7.2 on pages 257-258---*/

proc sort data=wheat; by variety moist; 
data wheat; set wheat; moistc=moist;

proc mixed scoring=8; 
   class variety moistc;
   model yield = moist / solution;
   random int moist / type=un sub=variety solution; 
   repeated moistc / subject=variety type=AR(1);
run;


/*---produces Output 7.3 on pages 259-260---*/

proc mixed scoring=8; 
   class variety;
   model yield = moist / solution;
   random int moist / sub=variety solution; 
run;

