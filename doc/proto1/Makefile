ALL_TARGETS = fdb_gen_ir.png ghcdump_ir.png q6_v1.png

all: $(ALL_TARGETS)


%.png: %.dot
	dot -Tpng $< -o $@
