OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES=-I .             # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

PROGNAME=miniml

# The list of object files for prog1
# OBJS=syntax.cmo parser.cmo lexer.cmo core.cmo typing.cmo main.cmo
OBJS=mySet.cmx syntax.cmx parser.cmx lexer.cmx environment.cmx typing.cmx eval.cmx
TESTOBJS=mySet.cmo syntax.cmo parser.cmo lexer.cmo environment.cmo typing.cmo eval.cmo

DEPEND += lexer.ml parser.ml

all: $(DEPEND) $(OBJS) main.cmx
	$(OCAMLOPT) -o $(PROGNAME) $(OCAMLFLAGS) $(OBJS) main.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

parser.ml parser.mli: parser.mly	
	@rm -f $@
	$(OCAMLYACC) -v $<
	@chmod -w $@

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

typeParser.ml typeParser.mli: typeParser.mly	
	@rm -f $@
	$(OCAMLYACC) -v $<
	@chmod -w $@

typeLexer.ml: typeLexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.ml parser.mli parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
