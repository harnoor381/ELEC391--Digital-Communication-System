//`timescale 1ps/1ps



module transmitter7_tb;

    reg clk = 1'b0;
    reg reset = 1'b0;
    reg [1:0] trans_in = 2'b00;
    reg read_ready;
    wire signed [15:0] trans_out;



//    trans_in,
   // trans_out,
  //  clk,
  //  reset,
   // read_ready


    // Instantiate the transmitter module
    transmitter7 #(
        .N(33),
        .DATA_WIDTH(16),
        .COEFF_WIDTH(16)
    ) dut (
        .clk(clk),
        .reset(reset),
        .trans_in(trans_in),
        .trans_out(trans_out),
        .read_ready(read_ready)
    );

    // Clock generation
    always forever begin
        #5 clk = ~clk;
    end

    // always forever begin
    //     #5;
    //     qpsk_out_imagbit = ~qpsk_out_imagbit;
    //     I = ~I;

    // end

    // Reset generation
    initial begin
        reset = 1; #10;
        reset = 0; read_ready = 1; trans_in = 2'b01; #10;
        read_ready = 0; #20;
        





        ///in_real_data = in_real_data + 2;
        //in_imag_data = in_imag_data + 2;
        #5000;
        $stop;
    end





    // // Stimulus generation
    // initial begin
    //     // Generate a short simulation
    //     for (i = 0; i < 10; i = i + 2) begin
    //         #5; // Wait for one clock cycle
    //     end
    //     // Stop the simulation
       
    // end

    // Display input and output values
    // always @(posedge clk) begin
    //     $display("Input: %d + %di, Output: %d + %di", in_real_data, in_imag_data, out_real_data, out_imag_data);
    // end

endmodule


