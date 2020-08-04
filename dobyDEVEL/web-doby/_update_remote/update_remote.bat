set TGT=/srv/users/sorenh@MATH/public_html/software/doBy

set THISDIR=%CD%
cd ..
set PARDIR=%CD%
cd %THISDIR%

echo open sorenh@Web01.math.aau.dk > script1.txt
echo synchronize remote %PARDIR% %TGT% >> script1.txt 
echo exit >> script1.txt

winscp /console /script=script1.txt
