
ALL_PRODS = $(shell ls -1 q*.hs | sed 's/\.hs/.o/')
GHC_FLAGS = -no-hs-main -no-link

all: $(ALL_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $<

clean:
	rm -v $(ALL_PRODS)
