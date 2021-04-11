
RS_DIR = rs
CARGO = cd $(RS_DIR) ; cargo
RUST_LIB = $(RS_DIR)/target/debug/libfdb.a

HS_DIR = hs
HS_MAKE = make -C $(HS_DIR)

all: test

tpch:
	$(HS_MAKE) tpch

clean:
	$(HS_MAKE) clean
	$(CARGO) clean

test: rs-lib
	$(HS_MAKE) test

rs-lib:
	$(CARGO) build
