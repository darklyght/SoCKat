DENSITY = den4096Mb
SPEEDGRADE = sg187E
WIDTH = x16
RANKS = SINGLE_RANK

PYTHON3_BIN = `which python3`
IVERILOG_BIN = `which iverilog`
VVP_BIN = `which vvp`

../device_parameters.scala:
	$(PYTHON3_BIN) ddr3_parameter_generation.py $(IVERILOG_BIN) $(VVP_BIN) $(DENSITY) $(SPEEDGRADE) $(WIDTH) $(RANKS)

.PHONY: ../device_parameters.scala