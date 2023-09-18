set dest=../../doBy-web/vignette

set doc=doBy-main
call Sweave2html.bat %doc%
cp %doc%.pdf  		%dest%/
cp %doc%.html 		%dest%/

set doc=doBy-lsmeans-etc
call Sweave2html.bat %doc%
cp %doc%.pdf  		%dest%/
cp %doc%.html 		%dest%/

cp ./figures/* %dest%/figures/
