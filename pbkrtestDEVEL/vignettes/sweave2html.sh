#!/bin/sh
## for i in `echo graphics/$1-[0-9]*.eps | sed -e 's/\.eps//g'`; do
##   convert graphics/$i.eps graphics/$i.png
## done
sed -e 's/\\begin{Sinput}/ { \\color{red}\\bf \\begin{verbatim}/
        s/\\end{Sinput}/ \\end{verbatim} }/
        s/\\begin{Soutput}/ { \\color{blue}\\bf \\begin{verbatim}/
        s/\\end{Soutput}/ \\end{verbatim} }/
        s/\\begin{Schunk}/ /
        s/\\end{Schunk}/ /
        s/\\hfill/ /
        s/\\smallskip//
        s/\\bigskip//
        s/\\medskip//
        s/\\hrule/\n\\hrule\n\n/
        s/\\vbox//
        s/\\begin{spacing}{.*}/\\begin{spacing}{}/
        s/\\vspace{[^}]*}//' $1.tex | tth -a -e2 -L$1 >$1.html
