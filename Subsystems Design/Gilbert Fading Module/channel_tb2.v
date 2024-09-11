//`timescale 1ps/1ps



module channel_tb2;


reg clk, CLOCK_50, reset;
//reg [15:0] channel_in;
reg [15:0] real_channel_in;
reg [15:0] imag_channel_in;
wire [15:0] real_channel_out;
wire [15:0] imag_channel_out;

//reg [6:0] random;

//instantiate channel
//channel DUT(.clk(clk), .CLOCK_50(CLOCK_50), .channel_in(channel_in), .channel_out(channel_out), .reset(reset));
//module channel(clk, CLOCK_50, real_channel_in, imag_channel_in, real_channel_out, imag_channel_out, reset);
channel DUT(.clk(clk), .CLOCK_50(CLOCK_50), .real_channel_in(real_channel_in), .imag_channel_in(imag_channel_in), .real_channel_out(real_channel_out), .imag_channel_out(imag_channel_out), .reset(reset));


//forever clock cycle
initial begin 
    clk=1'b0;
    #5;
    forever begin
        clk=1'b1; #5;
        clk=1'b0; #5;
    end
end

//just running a reset to initialize then running for extended period to see how random generator is affecting fsm switching
initial begin
    reset = 0;
    real_channel_in = 0;
    imag_channel_in = 0;
    #5;
    reset = 1; #10;
    reset = 0;
    #20;
    real_channel_in = 16'b0000000000100000;
    imag_channel_in = 16'b0000000000100000;
    #20;
    real_channel_in = 16'b0000010000000000;
    imag_channel_in = 16'b0000010000000000;
    #20;
    real_channel_in = 0;
    imag_channel_in = 0;

    #10000;
    $stop;

end





endmodule