module transmitter_reciever_tb();
  //reg [15:0] symb_in;//input to transmitter
  reg [1:0] symb_in;
  reg clk, reset;
  wire [15:0] symb_out; //output from transmitter, input to reciever
  wire [15:0] out;// output from reciever, input to downsampler
  wire [15:0] downsampled_out;//output from downsampler
  wire clk_1, clk_2, clk_3, clk_4, clk_5;
  reg read;

  //stuff inside brackets are top module signals, stuff with . is instantiated module signals
  //transmitter7 (trans_in,trans_out,clk,reset,read_ready)
  transmitter7 transmitter(.trans_in(symb_in), .trans_out(symb_out), .clk(clk), .reset(reset), .read_ready(read));

  //receiver(receive_in,receive_out,clk,reset)
  receiver receiver_instance(.receive_in(symb_out), .receive_out(out), .clk(clk), .reset(reset));

  //downsampler(in, out, clk, reset);
  downsampler downsampler(.in(out), .out(downsampled_out), .clk(clk), .reset(reset));//downsampled_out is final output


  initial begin
    reset = 1; #10;
    //reset = 0; read = 1; symb_in = 16'b0001011010100001; #10;
    reset = 0; read = 1; symb_in = 2'b01; #10;
    read = 0; 
    symb_in = 2'b00;
    #20;
    // read = 1; symb_in = 16'b1110100101011111; #10;
    // read = 0; #20;
    // read = 1; symb_in = 0; #10;
    // read = 0; #20;
    // read = 1; symb_in = 16'b1110100101011111; #10;
    // read = 0; #20;
    // read = 1; symb_in = 16'b0001011010100001; #10;
    // read = 0; #20;
    // read = 1; symb_in = 0; #10;
    // read = 0; #20;
    #400; $stop;
  end

  always begin
    clk = 0; #5; clk = 1; #5;
  end
endmodule