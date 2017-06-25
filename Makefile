lhs2tex:
	/home/joao/.cabal/bin/lhs2TeX -o LFCFDLazy.tex LFCFDLazy.lhs

pdf: lhs2tex
	pdflatex LFCFDLazy.tex

clean:
	rm LFCFDLazy.log LFCFDLazy.pdf LFCFDLazy.aux LFCFDLazy.tex LFCFDLazy.ptb LFCFDLazy.bcf LFCFDLazy.run.xml
