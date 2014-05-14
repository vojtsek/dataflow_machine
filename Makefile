all: machine

machine: Types.o main.o Core.o
	ghc -o machine Types.o main.o Core.o

Types.o: Types.hs
	ghc -c Types.hs

main.o: main.hs
	ghc -c main.hs

Core.o: Core.hs
	ghc -c Core.hs
