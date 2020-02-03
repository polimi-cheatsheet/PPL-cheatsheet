all: scheme.pdf haskell.pdf erlang.pdf PPL-cheatsheet.pdf

scheme.pdf: scheme.tex scheme.rkt
	latexmk -pdf $<
haskell.pdf: haskell.tex haskell.hs
	latexmk -pdf $<
erlang.pdf: erlang.tex erlang.erl
	latexmk -pdf $<
PPL-cheatsheet.pdf: scheme.pdf haskell.pdf erlang.pdf
	pdfunite $^ $@


clean:
	latexmk -C scheme.tex
	latexmk -C haskell.tex
	latexmk -C erlang.tex
	rm -f PPL-cheatsheet.pdf

.PHONY: all clean
