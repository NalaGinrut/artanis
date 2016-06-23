#General Makefile for latex
#Makes ps and pdf
#Can also make html
#Works on all *.tex files in directory
#$Id: Makefile.latex,v 1.4 2009-05-25 14:07:01 john Exp $

#initally written jcv 06/16/2005
#v1.0
#I had wrong version up that looked for main.tex
#now it does any .tex files in a directory
#09/15/2006
#v1.1
#It is no longer necessary to comment or uncomment bibtex.
#Makefile works if you need bibtex or not.
#05/02/2007
#v1.2
#11/05/2007
#v1.3
#Can now use pdf target to make pdf directly with pdflatex
#Also does everything in batchmode to clean up screen output
#05/15/2009
#v1.4
#make clean was missing a generated file before

#Copyright 2005-2009 John C. Vernaleo

#               (my_first_name)@netpurgatory.com
#                               or
#               (my_last_name)@astro.umd.edu
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#see readme_makefile.latex.txt for more information and gpl.txt for a
#copy of the GPL

#Possible targets are:
#all, html, words, clean, cleaner, htmlclean, pdf

RM = rm -rf

# by Kreutz: this improves the clean command by looking into all sub dirs
FINDANDRMAUX = rm -f `find . -type f | grep -E "(*.aux|*.bak)"`

BASE = ${wildcard *.tex}
FILES = ${BASE:%.tex=%.pdf}

LOPT =	-interaction=batchmode
LOPT2 = -shell-escape ${LOPT}

all:    pdf

%.pdf:	*.tex
	pdflatex ${LOPT} $*
	- makeindex $*
	- bibtex $*
	pdflatex ${LOPT} $*
	pdflatex ${LOPT} $*
	dvips -q -Ppdf -o$*.ps $*.dvi
	ps2pdf14 $*.ps

html:	${BASE:%.tex=%}

%:	%.tex
	latex2html $*

words:	${BASE:%.tex=%}

%:	%.tex
	pdflatex ${LOPT} $*
	dvips -q -o - $*.dvi | ps2ascii | wc -w
	@echo "words in $*"
	@echo ""

clean:
	${FINDANDRMAUX}
	${RM} ${BASE:%.tex=%.log}
	${RM} ${BASE:%.tex=%.dvi}	
	${RM} ${BASE:%.tex=%.ps}
	${RM} ${BASE:%.tex=%.pdf}
	${RM} ${BASE:%.tex=%.blg}
	${RM} ${BASE:%.tex=%.toc}
	${RM} ${BASE:%.tex=%.bbl}
	${RM} ${BASE:%.tex=%.aux}
	${RM} ${BASE:%.tex=%.lot}
	${RM} ${BASE:%.tex=%.lof}
	${RM} ${BASE:%.tex=%.out}

cleaner: clean
	${RM} ${BASE:%.tex=%.bbl}
	${RM} ${FILES}

htmlclean:
	${RM} ${BASE:%.tex=%/}

pdf:	${BASE:%.tex=%}

%:	%.tex
	pdflatex ${LOPT2} $*
	- bibtex $*
	pdflatex ${LOPT2} $*
	pdflatex ${LOPT2} $*

help:
	@echo Type "'make help'         To see this list"
	@echo Type "'make all'          To generate ps and pdf"
	@echo Type "'make pdf'          To generate pdf directly"
	@echo Type "'make html'         To generate html"
	@echo Type "'make clean'        To remove generated files except pdf"
	@echo Type "'make cleaner'      To remove ALL generated files"
	@echo Type "'make htmlclean'    To remove generated html files"
	@echo Type "'make words'        To get an estimate of word count"
