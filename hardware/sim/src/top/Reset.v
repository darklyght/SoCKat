module Reset #(
    parameter CYCLES = 1
) (
    input clk,
    output reset
);

    reg [31:0] counter;

    initial begin
        counter = 0;
    end

    always @ (posedge clk) begin
        if (counter < CYCLES) counter <= counter + 1;
    end

    assign reset = counter < CYCLES;

endmodule