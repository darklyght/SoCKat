MAIN += src/uart.c
CSRC += src/receive.c src/transmit.c
INC += include

uart.vpi: $(MAIN) $(CSRC) $(wildcard $(INC)/*.h)
	iverilog-vpi -I$(INC) $(MAIN) $(CSRC)

clean:
	rm *.o
	rm *.vpi

.PHONY: clean