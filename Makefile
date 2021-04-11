
BIN_DIR = bin
GHC_FLAGS = -outputdir $(BIN_DIR) -XMultiParamTypeClasses
TPCH_PRODS = $(shell ls -1 TPCH/*/Q*.hs | sed 's/\.hs/.o/')
TPCH_FLAGS = -no-hs-main -no-link
TEST_BIN = $(BIN_DIR)/fdb-test
TEST_MAIN_O = $(BIN_DIR)/Test/Main.o
TEST_MAIN_HS = Test/Main.hs
CFFI_O = $(BIN_DIR)/cffi.o
CFFI_H = Test/cffi.h
CFFI_C = Test/cffi.c
RFFI_A = $(BIN_DIR)/librffi.a
RFFI_RS = Test/rffi.rs

all: tpch test

tpch: $(TPCH_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $(TPCH_FLAGS) $<

clean:
	rm -v $(TPCH_PRODS)

test: compile-test
	$(TEST_BIN)

compile-test: $(CFFI_O) $(RFFI_A)
	ghc $(GHC_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS) $(CFFI_O) $(RFFI_A)

$(CFFI_O): $(CFFI_H) $(CFFI_C)
	gcc -c $(CFFI_C) -o $(CFFI_O)

$(RFFI_A): $(RFFI_RS)
	rustc --crate-type staticlib $(RFFI_RS) -o $(RFFI_A)

#$(TEST_BIN): $(TEST_MAIN_O)
#	cp $(TEST_MAIN_O) $(TEST_BIN)
#	chmod u+x $(TEST_BIN)

#$(TEST_MAIN_O): $(TEST_MAIN_HS)
#	ghc $(GHC_FLAGS) $(TEST_MAIN_HS)
