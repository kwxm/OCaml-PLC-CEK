all:
	dune build --profile release && cp _build/default/Main.exe ./cek && chmod +w cek

clean:
	dune clean && rm -f *.cmo *.cmi *.bytes *~

test1:
	dune exec --profile release ./Main.exe examples/basic/mul.ncbor

test2:
	dune exec --profile release ./Main.exe examples/fac.ncbor

test3:
	dune exec --profile release ./Main.exe examples/prime40.ncbor



# Build without Dune

OCAML_LIBDIR=$(HOME)/.opam/4.11.1/lib
# ... or whatever

LIBDIRS=-I $(OCAML_LIBDIR)/ocplib-endian -I $(OCAML_LIBDIR)/zarith
SRC=Absyn.ml CBOR.ml Cek.ml Decoder.ml Main.ml

# Native code version
cek-opt:
	ocamlopt $(LIBDIRS) ocplib_endian.cmxa zarith.cmxa $(SRC) -o cek

# Bytecode version
cek-bytecode:
	ocamlc   $(LIBDIRS) ocplib_endian.cma  zarith.cma  ${SRC} -o cek
