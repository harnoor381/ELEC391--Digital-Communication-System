`timescale 1ps/1ps



module channel_tb2;


reg clk, CLOCK_50, reset;
reg [15:0] channel_in;
wire [15:0] channel_out;

//reg [6:0] random;

//instantiate channel
channel DUT(.clk(clk), .CLOCK_50(CLOCK_50), .channel_in(channel_in), .channel_out(channel_out), .reset(reset));

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

    channel_in = 0;
    #20;

    reset = 1; #100;
    reset = 0; #5;

    
    


    #10000;
    $stop;

end





endmodule