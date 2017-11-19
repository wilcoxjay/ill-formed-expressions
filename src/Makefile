libs  = str
LIBS  = $(libs:%=-lib %)

DEP = *.ml *.mll *.mly
OCB = ocamlbuild -use-menhir -yaccflags --explain $(LIBS)


.PHONY: default
default: Main.native

%.native: %.ml $(DEP)
	$(OCB) $@

.PHONY: clean
clean:
	ocamlbuild -clean
