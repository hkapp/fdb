# Additional dependencies:
#  sqlite3 libsqlite3-dev

RS_DIR = rs
CARGO = cd $(RS_DIR) ; cargo
RUST_LIB = $(RS_DIR)/target/debug/libfdb.a

HS_DIR = hs
HS_MAKE = make -C $(HS_DIR)

DATA_DIR = data
DATA_SQLITE = $(DATA_DIR)/fdb.db
SQL_DIR = sql/dummy-data

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
	$(CARGO) build

rs-lib-release:
	$(CARGO) build --release

$(DATA_SQLITE): $(DATA_DIR) $(SQL_DIR)/create_foo.sql $(SQL_DIR)/create_pairs.sql
	echo '.exit' | sqlite3 $(DATA_SQLITE) -init $(SQL_DIR)/create_foo.sql
	echo '.exit' | sqlite3 $(DATA_SQLITE) -init $(SQL_DIR)/create_pairs.sql

$(DATA_DIR):
	mkdir $(DATA_DIR)
