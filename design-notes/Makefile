all : semantics.pdf

%.pdf : %.tex
	rubber -d $<

%.tex : %.lhs
	lhs2TeX $< > $@

clean:
	rm -f *.aux *.log *.pdf *.ptb