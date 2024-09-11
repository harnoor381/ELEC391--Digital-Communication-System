//OSR JSGJ

module tb_downsampler();
reg CLOCK_50;
reg [15:0] data_in;
wire [15:0] down_data_out;
//reg [2:0] counter;
reg reset;


downsampler DUT(.CLOCK_50(CLOCK_50),.data_in(data_in),.down_data_out(down_data_out), .reset(reset));

initial begin
forever begin
CLOCK_50 = 1'b0;
#10;
CLOCK_50 = 1'b1;
#10;
end

end

initial begin
reset =1;
#30 reset = 0;
data_in=16'd100;
#20;
data_in=16'd200;
#20;
data_in=16'd300;
#20;
data_in=16'd400;
#20;
data_in=16'd500;
#20;
data_in=16'd600; //should be output1
#20;
data_in=16'd700;
#20;
data_in=16'd800;
#20;
data_in=16'd900;
#20;
data_in=16'd1000;
#20;
data_in=16'd1100;
#20;
data_in=16'd1200; //should be output2
#20;
data_in=16'd1300;
#20;
data_in=16'd1400;
#20;
data_in=16'd1500;
#20;
data_in=16'd1600;
#20;
data_in=16'd1700;
#20;
data_in=16'd1800; //should be output3
#20;
data_in=16'd1900;
#20;
data_in=16'd2000;


#5;
end
endmodule
