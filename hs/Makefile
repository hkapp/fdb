
SRC_DIR = src
BIN_DIR = bin
GHC_FLAGS = -i$(SRC_DIR) -outputdir $(BIN_DIR) -XMultiParamTypeClasses
TPCH_PRODS = $(shell ls -1 $(SRC_DIR)/TPCH/*/Q*.hs | sed 's/\.hs/.o/')
TPCH_FLAGS = -no-hs-main -no-link
TEST_BIN = $(BIN_DIR)/fdb-test
TEST_MAIN_PATH = Test/Main
TEST_MAIN_O = $(BIN_DIR)/$(TEST_MAIN_PATH).o
TEST_MAIN_HS = $(SRC_DIR)/$(TEST_MAIN_PATH).hs

GHC_DUMP_OPTION_NAME1 = dump-prep
GHC_DUMP_OPTION_NAME2 = dump-hi
GHC_DUMP_FLAGS = -d$(GHC_DUMP_OPTION_NAME1) -d$(GHC_DUMP_OPTION_NAME2) -ddump-to-file  -fforce-recomp
ALL_HS_FILES = $(shell find $(SRC_DIR) -name "*.hs")
TEST_MAIN_DUMP1 = $(BIN_DIR)/$(SRC_DIR)/$(TEST_MAIN_PATH).$(GHC_DUMP_OPTION_NAME1)
TEST_MAIN_DUMP2 = $(BIN_DIR)/$(SRC_DIR)/$(TEST_MAIN_PATH).$(GHC_DUMP_OPTION_NAME2)
GHC_DUMP_PRODS = $(TEST_MAIN_DUMP1) $(TEST_MAIN_DUMP2)

RUST_LIB_DEV = ../rs/target/debug/libfdb.a
RUST_LIB_RELEASE = ../rs/target/release/libfdb.a
LIBS_DEV = $(RUST_LIB_DEV) -lsqlite3
LIBS_RELEASE = $(RUST_LIB_RELEASE) -lsqlite3

all: tpch test

tpch: $(TPCH_PRODS)

%.o: %.hs
	ghc $(GHC_FLAGS) $(TPCH_FLAGS) $<

clean:
	rm -r $(BIN_DIR)/*
	find $(SRC_DIR) -name "*.o" -exec rm {} \;
	find $(SRC_DIR) -name "*.hi" -exec rm {} \;
	find $(SRC_DIR) -name "*.hi" -exec rm {} \;
	find $(SRC_DIR) -name "*.hspp" -exec rm {} \;

# Only compile the Haskell code
compile: $(BIN_DIR)
	ghc $(GHC_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS) $(LIBS_DEV)

test: compile-test
	$(TEST_BIN)

test-speed: compile-test-speed
	$(TEST_BIN)

# Compile and generate the dump files
compile-test: $(RUST_LIB_DEV) $(BIN_DIR)
	ghc $(GHC_FLAGS) $(GHC_DUMP_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS) $(LIBS_DEV)

# Compile and generate the dump files (with the release rust)
compile-test-speed: $(RUST_LIB_RELEASE) $(BIN_DIR)
	ghc $(GHC_FLAGS) $(GHC_DUMP_FLAGS) -o $(TEST_BIN) $(TEST_MAIN_HS) $(LIBS_RELEASE)

$(BIN_DIR):
	mkdir $(BIN_DIR)

dump: compile-test

# Additional dependencies:
#  sqlite3 libsqlite3-dev
