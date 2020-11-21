all:
	dune build --profile release && cp _build/default/cek.exe ./cek

clean:
	dune clean && rm -f *.cmo *.cmi *.bytes *~

# cek:
# 	ocamlc -I ~/.opam/4.11.1/lib/cbor -I ~/.opam/4.11.1/lib/ocplib-endian/ ocplib_endian.cma cbor.cma cek.ml -o cek

test1:
	dune exec --profile release ./cek.bc examples/mul.ncbor

test2:
	dune exec --profile release ./cek.bc examples/fac.ncbor

test3:
	dune exec --profile release ./cek.bc examples/prime40.ncbor
