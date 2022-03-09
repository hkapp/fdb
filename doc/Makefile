ALL_TARGETS = fdb_gen_ir.png gcdump_ir.png

all: $(ALL_TARGETS)


fdb_gen_ir.png: fdb_gen_ir.dot
	dot -Tpng fdb_gen_ir.dot -o fdb_gen_ir.png

gcdump_ir.png: gcdump_ir.dot
	dot -Tpng gcdump_ir.dot -o gcdump_ir.png
