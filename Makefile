
HS_DIR = hs

all:
	make -C $(HS_DIR) all

tpch:
	make -C $(HS_DIR) tpch

clean:
	make -C $(HS_DIR) clean

test:
	make -C $(HS_DIR) test
