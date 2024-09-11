module encoder_tb; //testbench module
// signals in testbench
reg CLOCK_256;
reg reset;
reg [15:0] i2b;
wire [31:0] enc_bits;

con_enc DUT(                  //creating instance of Part2 module
    //connecting the ports of Part2 module with signals in testbench
    .CLOCK_256(CLOCK_256),
    .reset(reset),
    .i2b(i2b),
    .enc_bits(enc_bits)
);
initial begin
    CLOCK_256 = 0;
    forever #5 CLOCK_256 = ~CLOCK_256; //toggle every 5 time units
end

initial begin
    //apply reset
    /*expected output should be 32'b0 when reset high or input is 32'b0*/
    reset = 1'b1;
    i2b = {16{1'b0}};
    #10;
    reset = 1'b0;

    /*expected output should be 32'b0*/
    #10;
    reset = 1'b1;
    i2b = {16{1'b1}};
    #10;
    reset = 1'b0;

    //input test 1
    /*expected output should be 32'b11001111110011111100111111001111 == 3473870607*/
    #160; //wait for 16 clock cycles
    i2b = {16{1'b1}};
    #160;

    //input test 2
    /*expected output should be 32'b10101100101011001010110010101100 == 2902451724*/
    #160;
    i2b = {{4'b0000},{4'b1111},{4'b0000},{4'b1111}};
    #160;

    // Generate random test cases
    repeat (10) begin
        i2b = $random;
        #160;
    end

    #1000;
    $stop;


end
endmodule
