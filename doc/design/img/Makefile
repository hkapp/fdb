ALL_TARGETS = fdb_crates.png fdb_rustir.png interpreter_df.png interpreter_df3.png \
	df_join_fold_final.png

all: $(ALL_TARGETS)


%.png: %.dot
	dot -Tpng $< -o $@
