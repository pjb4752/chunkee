OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		ocamlbuild $(OCB_FLAGS)

OPAM_LIB_DIR=$(HOME)/.opam/system/lib

BINARY=chunkee
NATIVE=$(BINARY).native
BYTE=$(BINARY).byte

all: native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) $(NATIVE)

byte: sanity
	$(OCB) $(BYTE)

profile: sanity
	$(OCB) -tag profile $(NATIVE)

debug: sanity
	$(OCB) -tag debug $(BYTE)

sanity:
	ocamlfind query ounit

test: native byte
	$(OCB) -I test -pkg oUnit test.byte

.PHONY: all clean byte native profile debug sanity test
