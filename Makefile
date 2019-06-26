all: main

main: gol.ml
	dune build gol.exe

clean:
	dune clean