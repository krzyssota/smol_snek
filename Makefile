# Makefile generated by BNFC.

GHC        = ghc
GHC_OPTS	 = -fwarn-incomplete-patterns
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : SmolSnek

# Rules for building the parser.

AbsSmolSnekGrammar.hs LexSmolSnekGrammar.x ParSmolSnekGrammar.y PrintSmolSnekGrammar.hs SmolSnek.hs : smol_snek_grammar.cf
	bnfc --haskell --functor smol_snek_grammar.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

SmolSnek : AbsSmolSnekGrammar.hs LexSmolSnekGrammar.hs ParSmolSnekGrammar.hs PrintSmolSnekGrammar.hs SmolSnek.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsSmolSnekGrammar.hs AbsSmolSnekGrammar.hs.bak ComposOp.hs ComposOp.hs.bak DocSmolSnekGrammar.txt DocSmolSnekGrammar.txt.bak ErrM.hs ErrM.hs.bak LayoutSmolSnekGrammar.hs LayoutSmolSnekGrammar.hs.bak LexSmolSnekGrammar.x LexSmolSnekGrammar.x.bak ParSmolSnekGrammar.y ParSmolSnekGrammar.y.bak PrintSmolSnekGrammar.hs PrintSmolSnekGrammar.hs.bak SkelSmolSnekGrammar.hs SkelSmolSnekGrammar.hs.bak SmolSnek.hs SmolSnek.hs.bak XMLSmolSnekGrammar.hs XMLSmolSnekGrammar.hs.bak ASTSmolSnekGrammar.agda ASTSmolSnekGrammar.agda.bak ParserSmolSnekGrammar.agda ParserSmolSnekGrammar.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak smol_snek_grammar.dtd smol_snek_grammar.dtd.bak SmolSnek LexSmolSnekGrammar.hs ParSmolSnekGrammar.hs ParSmolSnekGrammar.info ParDataSmolSnekGrammar.hs Makefile


# EOF
