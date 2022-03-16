# Set up and generate TPCH data
# TPCH data generator can be found here: https://github.com/dragansah/tpch-dbgen
# Additional instructions on how to generate data: https://github.com/tvondra/pg_tpch

TPCHGEN_DIR = tpch-dbgen
# Some dependency on a file in the directory
# Much better than depending on the directory itself
TPCHGEN_SRC = $(TPCHGEN_DIR)/Makefile
TPCHGEN_GIT = git@github.com:dragansah/tpch-dbgen.git
TPCHGEN_BINNM = dbgen
TPCHGEN_BIN = $(TPCHGEN_DIR)/$(TPCHGEN_BINNM)
TPCHGEN_EXEC = cd $(TPCHGEN_DIR) ; ./$(TPCHGEN_BINNM)
TPCHGEN_OUTDIR = ..

DATA_DIR = ../../data
LINEITEM_V1 = $(DATA_DIR)/lineitem_v1.csv
ALL_DATA = $(LINEITEM_V1)

all: $(ALL_DATA)

clean:
	rm -f $(ALL_DATA)
	rm -rf $(TPCHGEN_DIR)

# dbgen
#   -s 0.001  -- scale factor 0.001 (generates ~1500 rows)
#   -T L      -- only table lineitem
#   -v        -- verbose
# We keep only 10 lines
$(LINEITEM_V1): $(TPCHGEN_BIN)
	$(TPCHGEN_EXEC) -s 0.001 -T L -v \
		| head -10 \
		> $(TPCHGEN_OUTDIR)/$(LINEITEM_V1)

# Amount of data generated for 'lineitem' per scale factor:
#
# Scale factor |  Size  | #rows
# -------------+--------+------
#      1       | 700 MB |  1.5 M
#      0.1     |  70 MB |  150 k
#      0.01    |   7 MB |   15 k
#      0.001   | 700 kB | 1500
#      0.0001  | 700  B |    1
#      0.00001 | 700  B |    1

$(TPCHGEN_BIN): $(TPCHGEN_SRC)
	cd $(TPCHGEN_DIR) ; make

$(TPCHGEN_SRC):
	git clone $(TPCHGEN_GIT)
