all:
	dune build --profile release && cp _build/default/Main.exe ./cek && chmod +w cek

clean:
	dune clean && rm -f *.cmo *.cmi *.bytes *~

test1:
	dune exec --profile release ./cek.exe examples/mul.ncbor

test2:
	dune exec --profile release ./cek.exe examples/fac.ncbor

test3:
	dune exec --profile release ./cek.exe examples/prime40.ncbor


SRC=Absyn.ml CBOR.ml Cek.ml Decoder.ml Main.ml

cek:
	ocamlc -I ~/.opam/4.11.1/lib/ocplib-endian/ -I ~/.opam/4.11.1/lib/zarith ocplib_endian.cma zarith.cma ${SRC} -o cek
