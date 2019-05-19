# All my source code
SRC := $(wildcard src/*.hs)

all: main clean

main: $(SRC)
	ghc -o $@ --make $^

clean:
	rm -rf src/*.o src/*.hi

