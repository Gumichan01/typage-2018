
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

Type.mli: Type.ml
	ocamlc -i $< > $@

Type.cmi: Type.mli
	ocamlc $(CFLAGS) $<

Type.cmo: Type.ml Type.mli Type.cmi
	ocamlc $(CFLAGS) $<

Expression.cmi: Expression.mli
	ocamlc $(CFLAGS) $<

Expression.cmo: Expression.mli Expression.cmi
	ocamlc $(CFLAGS) $<

Variable.mli: Variable.ml
	ocamlc -i $< > $@

Variable.cmi: Variable.mli
	ocamlc $(CFLAGS) $<

Variable.cmo: Variable.ml Variable.mli Variable.cmi Type.cmo
	ocamlc $(CFLAGS) $<

Unification.mli: Unification.ml
	ocamlc -i $< > $@

Unification.cmi: Unification.mli Type.cmo
	ocamlc $(CFLAGS) $<

Unification.cmo: Type.cmo Unification.ml Unification.mli Unification.cmi
	ocamlc $(CFLAGS) $<

Walgo.mli: Walgo.ml Expression.cmo Type.cmo
	ocamlc -i $< > $@

Walgo.cmi: Walgo.mli
	ocamlc $(CFLAGS) $<

Walgo.cmo: Walgo.ml Walgo.mli Walgo.cmi Unification.cmo Expression.cmo Type.cmo
	ocamlc $(CFLAGS) $<

clean:
	rm -f *.cmo *.cmi

mrproper: clean
	rm -f a.out
