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


SRC=Absyn.ml CBOR.ml Cek.ml Decoder.ml Main.ml

cek:
	ocamlopt -I ~/.opam/4.11.1/lib/ocplib-endian/ -I ~/.opam/4.11.1/lib/zarith ocplib_endian.cmxa zarith.cmxa ${SRC} -o cek

cek-bytecode:
	ocamlc -I ~/.opam/4.11.1/lib/ocplib-endian/ -I ~/.opam/4.11.1/lib/zarith ocplib_endian.cma zarith.cma ${SRC} -o cek
