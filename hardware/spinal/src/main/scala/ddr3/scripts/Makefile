DENSITY = den4096Mb
SPEEDGRADE = sg187E
WIDTH = x16
RANKS = SINGLE_RANK

PYTHON3_BIN = /usr/bin/python3

../device_parameters.scala:
	$(PYTHON3_BIN) ddr3_parameter_generation.py $(DENSITY) $(SPEEDGRADE) $(WIDTH) $(RANKS)

.PHONY: ../device_parameters.scala