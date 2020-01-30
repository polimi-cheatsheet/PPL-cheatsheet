all: scheme.pdf haskell.pdf erlang.pdf

scheme.pdf: scheme.tex scheme.rkt
	latexmk -pdf $<
haskell.pdf: haskell.tex haskell.hs
	latexmk -pdf $<
erlang.pdf: erlang.tex erlang.erl
	latexmk -pdf $<

clean:
	latexmk -C scheme.tex
	latexmk -C haskell.tex
	latexmk -C erlang.tex

.PHONY: all clean
