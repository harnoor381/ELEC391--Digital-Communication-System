module BER_cal_tb;

    // Testbench signals
    reg clk;
    reg reset;
    reg [15:0] data_out_clear;
    reg [15:0] buff_out;
    wire [31:0] error_count;
    wire [31:0] total_bits;

    // Instantiate the BER calculator
    BER_cal DUT (
        .clk(clk),
        .reset(reset),
        .data_out_clear(data_out_clear),
        .buff_out(buff_out),
        .error_count(error_count),
        .total_bits(total_bits)
    );

    // Clock generation
    
    initial begin
    clk = 0;
    forever #5 clk = ~clk; //toggle every 5 time units
    end

    initial begin
        // Initialize signals
        transmitted_data = 16'b0;
        received_data = 16'b0;

        // Apply reset
        #10; 
        reset = 0;

        // Test cases
        #10 transmitted_data = 16'b1010101010101010; received_data = 16'b1010101010101010;
        #10 transmitted_data = 16'b1111000011110000; received_data = 16'b1111000011111111;
        #10 transmitted_data = 16'b0000111100001111; received_data = 16'b0000000000000000;
        #10 transmitted_data = 16'b1111111111111111; received_data = 16'b1111111111111110;
        #10 transmitted_data = 16'b0000000000000000; received_data = 16'b0000000000000001;

        // End of test
        #50;
         $stop;
    end
endmodule