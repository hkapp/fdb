
BIN_DIR = bin
GHC_FLAGS = -outputdir $(BIN_DIR) -XMultiParamTypeClasses
TPCH_PRODS = $(shell ls -1 TPCH/*/q*.hs | sed 's/\.hs/.o/')
TPCH_FLAGS = -no-hs-main -no-link

all: tpch

tpch: $(TPCH_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $(TPCH_FLAGS) $<

clean:
	rm -v $(TPCH_PRODS)
