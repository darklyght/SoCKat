`timescale 1ns/1ps

module Clock #(
    parameter PERIOD = 10,
    parameter PHASE = 0
) (
    output clk
);

    reg clk_register = PHASE * PERIOD / 180 > PERIOD;

    initial begin
        #(PERIOD / 2 - PHASE * PERIOD / 360) clk_register = ~clk_register;
    end

    always begin
        #(PERIOD / 2) clk_register = ~clk_register;
    end

    assign clk = clk_register;

endmodule