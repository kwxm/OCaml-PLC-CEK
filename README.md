## OCaml-PLC-CEK

A basic CEK machine for Plutus Core, implemented in OCaml.

### Installation 
To compile the machine, [install OCaml](https://ocaml.org/docs/install.html) (probably
with OPAM) and then type `make`.  That should leave an executable called `cek`
in the top-level directory.


### Running the program

The `cek` program operates in two different ways.  You can type

```
  cek <file>
```
to run a program once: the result and the execution time will be printed.

You can also type
```
  cek -t <n> <file>
```

to run the program `n` times: the execution times (in seconds) will be printed
each time, and at the end the average time will be printed (but not the program
result).  This is a quick way to get benchmarking data.

### Input data
The executable consumes Plutus Core programs in CBOR format (serialised with
Names, not de Bruijn indices).  You can get these by using

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
