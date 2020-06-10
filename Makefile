
BIN_DIR = bin
GHC_FLAGS = -outputdir $(BIN_DIR) -XMultiParamTypeClasses
TPCH_PRODS = $(shell ls -1 TPCH/*/Q*.hs | sed 's/\.hs/.o/')
TPCH_FLAGS = -no-hs-main -no-link
TEST_BIN = $(BIN_DIR)/fdb-test
TEST_MAIN_O = $(BIN_DIR)/Test/Main.o
TEST_MAIN_HS = Test/Main.hs

all: tpch test

tpch: $(TPCH_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $(TPCH_FLAGS) $<

clean:
	rm -v $(TPCH_PRODS)

test: compile-test
	$(TEST_BIN)

compile-test:
	ghc $(GHC_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS)

#$(TEST_BIN): $(TEST_MAIN_O)
#	cp $(TEST_MAIN_O) $(TEST_BIN)
#	chmod u+x $(TEST_BIN)

#$(TEST_MAIN_O): $(TEST_MAIN_HS)
#	ghc $(GHC_FLAGS) $(TEST_MAIN_HS)
