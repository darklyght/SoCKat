MAIN += src/uart.c
CSRC += src/receive.c src/transmit.c
INC += include

IVERILOG_VPI_BIN = /usr/local/bin/iverilog-vpi

uart.vpi: $(MAIN) $(CSRC) $(wildcard $(INC)/*.h)
	$(IVERILOG_VPI_BIN) -I$(INC) $(MAIN) $(CSRC)

clean:
	rm *.o
	rm *.vpi

.PHONY: clean