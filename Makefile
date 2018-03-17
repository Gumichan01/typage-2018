
CC=ocaml
CFLAGS=-c
OFLAGS=-a

all:

ChurchType.mli: ChurchType.ml
	ocamlc -i $< > $@

ChurchType.cmi: ChurchType.mli
	ocamlc $(CFLAGS) $<

ChurchType.cmo: ChurchType.ml ChurchType.mli ChurchType.cmi
	ocamlc $(CFLAGS) $<

Conversion.mli: Conversion.ml
	ocamlc -i $< > $@

Conversion.cmi: Conversion.mli
	ocamlc $(CFLAGS) $<

Conversion.cmo: Conversion.ml Conversion.mli Conversion.cmi ChurchType.cmo
	ocamlc $(CFLAGS) $<

Unification.mli: Unification.ml
	ocamlc -i $< > $@

Unification.cmi: Unification.mli
	ocamlc $(CFLAGS) $<

Unification.cmo: Unification.ml Unification.mli Unification.cmi ChurchType.cmo
	ocamlc $(CFLAGS) $<

Walgo.mli: Walgo.ml
	ocamlc -i $< > $@

Walgo.cmi: Walgo.mli
	ocamlc $(CFLAGS) $<

Walgo.cmo: Walgo.ml Walgo.mli Walgo.cmi Unification.cmo ChurchType.cmo
	ocamlc $(CFLAGS) $<

clean:
	rm -f *.cmo *.cmi
