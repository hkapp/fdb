ALL_TARGETS = fdb_gen_ir.png ghcdump_ir.png

all: $(ALL_TARGETS)


fdb_gen_ir.png: fdb_gen_ir.dot
	dot -Tpng fdb_gen_ir.dot -o fdb_gen_ir.png

ghcdump_ir.png: ghcdump_ir.dot
	dot -Tpng ghcdump_ir.dot -o ghcdump_ir.png
