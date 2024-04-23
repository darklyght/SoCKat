create_clock -add -period 8.477 -name uart_clock [get_nets topClockArea_uartClock_CLKOUT0]

set_clock_groups -asynchronous -group [get_clocks uart_clock]