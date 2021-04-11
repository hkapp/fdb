
SRC_DIR = src
BIN_DIR = bin
GHC_FLAGS = -i$(SRC_DIR) -outputdir $(BIN_DIR) -XMultiParamTypeClasses
TPCH_PRODS = $(shell ls -1 $(SRC_DIR)/TPCH/*/Q*.hs | sed 's/\.hs/.o/')
TPCH_FLAGS = -no-hs-main -no-link
TEST_BIN = $(BIN_DIR)/fdb-test
TEST_MAIN_O = $(BIN_DIR)/Test/Main.o
TEST_MAIN_HS = $(SRC_DIR)/Test/Main.hs
RUST_LIB = ../rs/target/debug/libfdb.a

all: tpch test

tpch: $(TPCH_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $(TPCH_FLAGS) $<

clean:
	rm -r $(BIN_DIR)/*

test: compile-test
	$(TEST_BIN)

compile-test: $(CFFI_O) $(RUST_LIB)
	ghc $(GHC_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS) $(CFFI_O) $(RUST_LIB)
