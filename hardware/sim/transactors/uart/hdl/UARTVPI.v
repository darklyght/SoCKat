module UARTVPI #(
    parameter NAME = "uart"
) (
    input               clk,
    output reg  [7:0]   transmit_data,
    input               transmit_ready,
    output reg          transmit_valid,
    input   [7:0]       receive_data,
    output              receive_ready,
    input               receive_valid
);

    initial begin
        $uart_create(NAME);
    end

    always @ (posedge clk) begin
        if (transmit_ready) transmit_valid <= $transmit_valid(NAME);
        if (transmit_ready) transmit_data <= $transmit_data(NAME);
        if (receive_valid) $receive_data(NAME, receive_data);
    end

    assign receive_ready = 'b1;

endmodule