
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

IType.mli: IType.ml
	ocamlc -i $< > $@

IType.cmi: IType.mli
	ocamlc $(CFLAGS) $<

IType.cmo: IType.ml IType.mli IType.cmi
	ocamlc $(CFLAGS) $<

Conversion.mli: Conversion.ml
	ocamlc -i $< > $@

Conversion.cmi: Conversion.mli
	ocamlc $(CFLAGS) $<

Conversion.cmo: Conversion.ml Conversion.mli Conversion.cmi IType.cmo
	ocamlc $(CFLAGS) $<

Unification.mli: Unification.ml
	ocamlc -i $< > $@

Unification.cmi: Unification.mli
	ocamlc $(CFLAGS) $<

Unification.cmo: Unification.ml Unification.mli Unification.cmi IType.cmo
	ocamlc $(CFLAGS) $<

Walgo.mli: Walgo.ml
	ocamlc -i $< > $@

Walgo.cmi: Walgo.mli
	ocamlc $(CFLAGS) $<

Walgo.cmo: Walgo.ml Walgo.mli Walgo.cmi Unification.cmo IType.cmo
	ocamlc $(CFLAGS) $<

clean:
	rm -f *.cmo *.cmi

mrproper: clean
	rm -f a.out
