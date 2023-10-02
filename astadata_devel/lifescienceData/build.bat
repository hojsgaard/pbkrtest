rem building the package
 R CMD BATCH build.R

rem writing the package to the server	

c:
cd c:\temp
winscp  /console /script=d:\dataRepDEVEL\build_winscp.txt


rem removing the temp packages

rem rmdir /S /Q c:\temp\rpackage2.11
rem dir /S /Q c:\temp\rpackage2.12
