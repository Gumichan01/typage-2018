
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

Expression.cmi: Expression.mli
	ocamlc $(CFLAGS) $<

Expression.cmo: Expression.mli Expression.cmi
	ocamlc $(CFLAGS) $<

Variable.mli: Variable.ml
	ocamlc -i $< > $@

Variable.cmi: Variable.mli
	ocamlc $(CFLAGS) $<

Variable.cmo: Variable.ml Variable.mli Variable.cmi IType.cmo
	ocamlc $(CFLAGS) $<

Unification.mli: Unification.ml
	ocamlc -i $< > $@

Unification.cmi: Unification.mli IType.cmo
	ocamlc $(CFLAGS) $<

Unification.cmo: IType.cmo Unification.ml Unification.mli Unification.cmi
	ocamlc $(CFLAGS) $<

Walgo.mli: Walgo.ml Expression.cmo IType.cmo
	ocamlc -i $< > $@

Walgo.cmi: Walgo.mli
	ocamlc $(CFLAGS) $<

Walgo.cmo: Walgo.ml Walgo.mli Walgo.cmi Unification.cmo Expression.cmo IType.cmo
	ocamlc $(CFLAGS) $<

clean:
	rm -f *.cmo *.cmi

mrproper: clean
	rm -f a.out
