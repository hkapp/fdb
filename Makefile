
RS_DIR = rs
CARGO = cd $(RS_DIR) ; cargo
RUST_LIB = $(RS_DIR)/target/debug/libfdb.a

HS_DIR = hs
HS_MAKE = make -C $(HS_DIR)

all: compile

tpch:
	$(HS_MAKE) tpch

clean:
	$(HS_MAKE) clean
	$(CARGO) clean

test: rs-lib
	$(HS_MAKE) test

test-speed: rs-lib-release
	$(HS_MAKE) test

compile: rs-lib
	$(HS_MAKE) compile

rs-lib:
	$(CARGO) build

rs-lib-release:
	$(CARGO) build --release
