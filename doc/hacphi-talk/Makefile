# DIAGRAMS = $(patsubst %.hs,%.eps,$(wildcard diagrams/[a-z]*.hs))
IMAGES = $(wildcard images/*.{jpg,pdf})

all : diagrams-hacphi-20100522.pdf

%.pdf : %.lhs $(IMAGES)
	rubber -f -d $<

# diagrams/%.eps: diagrams/%.hs diagrams/SpeciesDiagrams.o diagrams/TreeDiagrams.o
# 	cd diagrams && runhaskell ../$< && ps2eps -f ../$(patsubst %.hs,%.ps,$<)

# diagrams/%.o: diagrams/%.hs
# 	ghc -c $< -o $@

# .SECONDARY : $(DIAGRAMS) species-pearl.lhs
  # preserve compiled .eps and .lhs files to save time on later builds

clean: 
	rm -f *~ *.aux *.ptb *.log *.pdf *blx.bib *.dvi *.hi *.o *.bbl *.blg *.nav *.out *.snm *.toc *.tex
#	rm -f diagrams/*.ps