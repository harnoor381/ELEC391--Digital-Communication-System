module QPSK_demod_tb; // Testbench module

// Signals in testbench
reg CLOCK_256;
reg reset;
reg [1:0] I;
reg [1:0] Q;
wire [3:0] demod_bits;
reg[2:0] q_data;
reg[1:0] i_data;

// Instantiate the QPSK_demod module
QPSK_demod DUT (
    .CLOCK_256(CLOCK_256),
    .reset(reset),
    .I(I),
    .Q(Q),
    .demod_bits(demod_bits)
);

// Clock generation
initial begin
    CLOCK_256 = 0;
    forever #5 CLOCK_256 = ~CLOCK_256; // Toggle every 5 time units
end

// Test sequence
initial begin
    // Apply reset
    reset = 1'b1;
    I = 2'b00;
    Q = 2'b00;
    #10;
    reset = 1'b0;

    // Test case 1: I = 1, Q = 0
    #10;
    I = 2'b01;
    Q = 2'b00;
    #10;
    if (demod_bits !== 4'b0000) $display("Test case 1 failed");

    // Test case 2: I = 0, Q = 1
    #10;
    I = 2'b00;
    Q = 2'b01;
    #10;
    if (demod_bits !== 4'b0001) $display("Test case 2 failed");

    // Test case 3: I = -1, Q = 0
    #10;
    I = 2'b11; // 2'b11 represents -1 in 2's complement form
    Q = 2'b00;
    #10;
    if (demod_bits !== 4'b0011) $display("Test case 3 failed");

    // Test case 4: I = 0, Q = -1
    #10;
    I = 2'b00;
    Q = 2'b11; // 2'b11 represents -1 in 2's complement form
    #10;
    if (demod_bits !== 4'b0010) $display("Test case 4 failed");

    // Test default case
    #10;
    I = 2'b10; // Undefined state
    Q = 2'b10; // Undefined state
    #10;
    if (demod_bits !== 4'b0000) $display("Default case failed");



    // End of test
    #50;
    $stop;
end

endmodule
