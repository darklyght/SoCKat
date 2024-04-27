ROOT = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

HARD_SPINAL_DIRECTORY = $(ROOT)/hardware/spinal

HARD_DDR3_PARAMETERS = $(HARD_SPINAL_DIRECTORY)/src/main/scala/ddr3/device_parameters.scala
HARD_DDR3_DENSITY = den4096Mb
HARD_DDR3_SPEEDGRADE = sg187E
HARD_DDR3_WIDTH = x16
HARD_DDR3_RANKS = DUAL_RANK

HARD_SRC_DIRECTORY = $(ROOT)/hardware/src
HARD_SRC_LIST = $(HARD_SRC_DIRECTORY)/hdl/top/Top.v
HARD_SRC_SIM_LIST = $(HARD_SRC_DIRECTORY)/hdl/top/TopSimulationModel.v

HARD_SIM_DIRECTORY = $(ROOT)/hardware/sim
HARD_SIM_TRANSACTORS_DIRECTORY = $(dir $(wildcard $(HARD_SIM_DIRECTORY)/transactors/*/))
HARD_SIM_TRANSACTORS_LIST = $(subst /,,$(subst $(HARD_SIM_DIRECTORY)/transactors/,,$(HARD_SIM_TRANSACTORS_DIRECTORY)))
HARD_SIM_SRC_DIRECTORY = $(HARD_SIM_DIRECTORY)/src
HARD_SIM_SRC_LIST = $(wildcard $(HARD_SIM_SRC_DIRECTORY)/*/*.v)
HARD_SIM_XILINX_LIB = $(HARD_SIM_DIRECTORY)/lib/XilinxUnisimLibrary/verilog/src/unisims
HARD_SIM_XILINX_SRC = $(HARD_SIM_DIRECTORY)/lib/XilinxUnisimLibrary/verilog/src/glbl.v
HARD_SIM_VVP = $(HARD_SIM_DIRECTORY)/TopSimulationModel.vvp

HARD_BUILD_DIRECTORY = $(ROOT)/hardware/build
HARD_SYN_DCP = $(HARD_BUILD_DIRECTORY)/post_synth.dcp
HARD_PNR_DCP = $(HARD_BUILD_DIRECTORY)/post_route.dcp
HARD_BIT_FILE = $(HARD_BUILD_DIRECTORY)/top.bit

SBT_BIN = /home/darklyght/.local/share/coursier/bin/sbt
PYTHON3_BIN = /usr/bin/python3
IVERILOG_BIN = /usr/local/bin/iverilog
VVP_BIN = /usr/local/bin/vvp
VIVADO_BIN = /tools/Xilinx/Vivado/2023.1/bin/vivado

ddr3: $(HARD_DDR3_PARAMETERS)

$(HARD_DDR3_PARAMETERS):
	$(MAKE) -C $(HARD_SPINAL_DIRECTORY)/src/main/scala/ddr3/scripts PYTHON3_BIN=$(PYTHON3_BIN) IVERILOG_BIN=$(IVERILOG_BIN) DENSITY=$(HARD_DDR3_DENSITY) SPEEDGRADE=$(HARD_DDR3_SPEEDGRADE) WIDTH=$(HARD_DDR3_WIDTH) RANKS=$(HARD_DDR3_RANKS)

regression: $(HARD_DDR3_PARAMETERS)
	cd $(HARD_SPINAL_DIRECTORY) && $(SBT_BIN) "runMain sockat.top.Regression"

transactors: $(HARD_SIM_TRANSACTORS_DIRECTORY)

$(HARD_SIM_TRANSACTORS_DIRECTORY):
	$(MAKE) -C $@

simulation: $(HARD_SIM_VVP)
	$(VVP_BIN) -n $(addprefix -M ,$(HARD_SIM_TRANSACTORS_DIRECTORY)) $(addprefix -m ,$(HARD_SIM_TRANSACTORS_LIST)) $(HARD_SIM_VVP)

$(HARD_SIM_VVP): $(HARD_SRC_SIM_LIST) $(HARD_SIM_TRANSACTORS_DIRECTORY) $(HARD_SIM_SRC_LIST)
	$(IVERILOG_BIN) -g2012 -o $(HARD_SIM_VVP) $(addprefix -y ,$(HARD_SIM_XILINX_LIB)) $(addprefix -y ,$(addsuffix hdl,$(HARD_SIM_TRANSACTORS_DIRECTORY))) $(HARD_SIM_SRC_LIST) $(HARD_SRC_SIM_LIST) $(HARD_SIM_XILINX_SRC)

$(HARD_SRC_SIM_LIST): $(HARD_DDR3_PARAMETERS)
	cd $(HARD_SPINAL_DIRECTORY) && $(SBT_BIN) "runMain sockat.top.TopSimulation"

synthesis: $(HARD_SYN_DCP)

$(HARD_SYN_DCP): $(HARD_SRC_LIST)
	$(VIVADO_BIN) -mode batch -source $(HARD_BUILD_DIRECTORY)/scripts/syn.tcl -tclarg $(ROOT)

pnr: $(HARD_PNR_DCP)

$(HARD_PNR_DCP): $(HARD_SYN_DCP)
	$(VIVADO_BIN) -mode batch -source $(HARD_BUILD_DIRECTORY)/scripts/pnr.tcl -tclarg $(ROOT)

$(HARD_BIT_FILE): $(HARD_PNR_DCP)

program: $(HARD_BIT_FILE)
	$(VIVADO_BIN) -mode batch -source $(HARD_BUILD_DIRECTORY)/scripts/program.tcl -tclarg $(ROOT)

$(HARD_SRC_LIST):
	cd $(HARD_SPINAL_DIRECTORY) && $(SBT_BIN) "runMain sockat.top.TopVerilog"

.PHONY: ddr3 regression transactors $(HARD_SIM_TRANSACTORS_DIRECTORY) $(HARD_SRC_SIM_LIST) synthesis pnr program $(HARD_SRC_LIST)