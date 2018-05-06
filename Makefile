
CC=ocaml
CFLAGS=-c -g -rectypes
OFLAGS=
EXE=itype

all: $(EXE)
	@echo ""

$(EXE): Type.cmo Unification.cmo Expression.cmo Expression.cmo Walgo.cmo
	ocamlc $(OFLAGS) $^ -o $@

# ChurchType is unused
ChurchType.mli: ChurchType.ml
	ocamlc -i $(CFLAGS) $< > $@

ChurchType.cmi: ChurchType.mli
	ocamlc $(CFLAGS) $<

ChurchType.cmo: ChurchType.ml ChurchType.mli ChurchType.cmi
	ocamlc $(CFLAGS) $<

Type.mli: Type.ml
	ocamlc -i $(CFLAGS) $< > $@

Type.cmi: Type.mli
	ocamlc $(CFLAGS) $<

Type.cmo: Type.ml Type.mli Type.cmi
	ocamlc $(CFLAGS) $<

Expression.cmi: Expression.mli
	ocamlc $(CFLAGS) $<

Expression.cmo: Expression.ml Expression.mli Expression.cmi
	ocamlc $(CFLAGS) $<

# Variable is unused
Variable.mli: Variable.ml
	ocamlc -i $< > $@

Variable.cmi: Variable.mli
	ocamlc $(CFLAGS) $<

Variable.cmo: Variable.ml Variable.mli Variable.cmi Type.cmo
	ocamlc $(CFLAGS) $<

Unification.mli: Unification.ml
	ocamlc -i $(CFLAGS) $< > $@

Unification.cmi: Unification.mli Type.cmo
	ocamlc $(CFLAGS) $<

Unification.cmo: Unification.ml Unification.mli Unification.cmi Type.cmo
	ocamlc $(CFLAGS) $<

Walgo.mli: Walgo.ml Unification.cmo Expression.cmo Type.cmo
	ocamlc -i $(CFLAGS) $< > $@

Walgo.cmi: Walgo.mli
	ocamlc $(CFLAGS) $<

Walgo.cmo: Walgo.ml Walgo.mli Walgo.cmi Unification.cmo Expression.cmo
	ocamlc $(CFLAGS) $<

clean:
	rm -f *.cmo *.cmi

mrproper: clean
	rm -f a.out
