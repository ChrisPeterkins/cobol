COBC = cobc
FLAGS = -x -I copybooks -std=default
SRC = src
BIN = bin

PROGRAMS = acctmgr txnproc dayend rptgen seedload

all: $(addprefix $(BIN)/,$(PROGRAMS))

$(BIN)/%: $(SRC)/%.cbl copybooks/*.cpy | $(BIN)
	$(COBC) $(FLAGS) -o $@ $<

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)/*

cleandata:
	rm -f data/*.dat

reset: clean cleandata

.PHONY: all clean cleandata reset
