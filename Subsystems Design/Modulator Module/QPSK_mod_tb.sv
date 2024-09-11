module QPSK_mod_tb;
//signals in testbench
reg CLOCK_256;
reg reset;
reg [31:0] enc_bits;
wire [1:0] I;
wire [1:0] Q;

QPSK_mod DUT (
    .CLOCK_256(CLOCK_256),
    .reset(reset),
    .enc_bits(enc_bits),
    .I(I),
    .Q(Q)
);
initial begin
    CLOCK_256 = 0;
    forever #5 CLOCK_256 = ~CLOCK_256;
end
initial begin
    //apply reset
    /*expected output should be 32'b0 when reset high or input is 32'b0*/
    reset = 1'b1;
    enc_bits = {32{1'b0}};
    #160;
    reset = 1'b0;

    /*expected output should be 32'b0*/
    #10;
    reset = 1'b1;
    enc_bits = {32{1'b1}};
    #160;
    reset = 1'b0;

    //input test 1
    #160;
    enc_bits = {{16{1'b1}}, {16{1'b0}}};
    #160;

    //input test 2
    #160;
    enc_bits = {{4{1'b1}}, {4{1'b0}}, {4{1'b1}}, {4{1'b0}}, {4{1'b1}}, {4{1'b0}}, {4{1'b1}}, {4{1'b0}}};
    #160; 

    // Generate random test cases
    repeat (10) begin
        enc_bits = $random;
        #160;
    end

    $stop;
end
endmodule