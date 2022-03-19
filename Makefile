# Additional dependencies:
#  sqlite3 libsqlite3-dev

# VARIABLES

RS_DIR = rs
CARGO = cd $(RS_DIR) ; cargo
RUST_LIB = $(RS_DIR)/target/debug/libfdb.a

HS_DIR = hs
HS_MAKE = make -C $(HS_DIR)

SQLITE = echo '.exit' | sqlite3 $(DATA_SQLITE)

DATA_DIR = data
DATA_SQLITE = $(DATA_DIR)/fdb.db
SQL_ROOT = sql
SQL_DUMMY = $(SQL_ROOT)/dummy-data
SQL_TPCH = $(SQL_ROOT)/tpch-sql

TOKEN_DIR = $(DATA_DIR)/token_dir
TOKEN_FOO = $(DATA_DIR)/token_foo
TOKEN_PAIRS = $(DATA_DIR)/token_pairs
# lineitem_v1
TOKEN_LIV1 = $(DATA_DIR)/token_liv1
# lineitem 10 rows
TOKEN_LIR10 = $(DATA_DIR)/token_lir10
ALL_TOKENS = $(TOKEN_FOO) $(TOKEN_PAIRS) $(TOKEN_LIV1)

DATA_LIR10 = $(DATA_DIR)/lineitem_v1.csv

# TARGETS

all: compile

tpch:
	$(HS_MAKE) tpch

clean:
	$(HS_MAKE) clean
	$(CARGO) clean

test: rs-lib $(DATA_SQLITE)
	$(HS_MAKE) test

test-speed: rs-lib-release $(DATA_SQLITE)
	$(HS_MAKE) test-speed

compile: rs-lib
	$(HS_MAKE) compile

rs-lib:
	$(CARGO) rustc -- -Awarnings

rs-lib-release:
	$(CARGO) rustc -- -Awarnings --release

# SQLITE SETUP

$(DATA_SQLITE): $(ALL_TOKENS)

$(TOKEN_DIR):
	mkdir -p $(DATA_DIR)
	touch $(TOKEN_DIR)

$(TOKEN_FOO): $(TOKEN_DIR) $(SQL_DUMMY)/create_foo.sql
	$(SQLITE) -init $(SQL_DUMMY)/create_foo.sql
	touch $(TOKEN_FOO)

$(TOKEN_PAIRS): $(TOKEN_DIR) $(SQL_DUMMY)/create_pairs.sql
	$(SQLITE) -init $(SQL_DUMMY)/create_pairs.sql
	touch $(TOKEN_PAIRS)

$(TOKEN_LIV1): $(TOKEN_DIR) $(SQL_DUMMY)/create_lineitem_v1.sql
	$(SQLITE) -init $(SQL_DUMMY)/create_lineitem_v1.sql
	$(SQLITE) -init $(SQL_DUMMY)/q6_v1.sql > $(TOKEN_LIV1)
	cat $(TOKEN_LIV1)

$(TOKEN_LIR10): $(TOKEN_DIR) $(DATA_LIR10) $(SQL_TPCH)/tpch-create.sqlite.sql
	$(SQLITE) -init $(SQL_TPCH)/tpch-create.sqlite.sql
	$(SQLITE) -cmd '.mode csv' -cmd '.separator |' -cmd '.import $(DATA_LIR10) lineitem'
	touch $(TOKEN_LIR10)

$(DATA_LIR10):
	cd sql/tpch-gen ; make
