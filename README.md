A basic CEK machine for Plutus Core, implemented in OCaml.

To compile this, (install OCaml)[https://ocaml.org/docs/install.html] (probably
with OPAM) and then type `make`.  That should leave an executable called `cek`
in this directory.  This consumes Plutus Core programs in CBOR format (serialised
with `Name`s, not de Bruijn indices.  You can get these by using

```
  plc convert --of cbor-named -i <input file> -o <output file>
```

on textual Plutus Core programs, such as the ones in
`plutus-benchmark/validation/data`: see the README files in that directory for instructions
on how to assemble the separate files into executable validators.

You can also use the `nofib-exe` command to generate suitable CBOR from the `nofib` examples:
for example

```
  nofib-exe dumpCBORnamed prime P60
```

See `plc convert --help` and `nofib-exe --help` for more information.
