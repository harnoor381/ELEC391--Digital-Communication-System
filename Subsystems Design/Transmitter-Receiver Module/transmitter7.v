//`timescale 1ps/1ps


//might need to put the coefficients in as inputs?
//where to add coefficients, where to add 

// module matched_filter(clk,reset,data_in,data_out);


// endmodule

module transmitter7 #(
    parameter N = 33, // Number of filter coefficients
    parameter DATA_WIDTH = 16, // Width of input/output data
    parameter COEFF_WIDTH = 16 // Width of coefficients
) (
    //input wire [1:0] I, //real output bits from qpsk
    //input wire [1:0] Q, //imaginary output bits from qpsk
    trans_in,
    trans_out,
    clk,
    reset,
    read_ready
    //input wire signed [DATA_WIDTH-1:0] in_real_data,
    //input wire signed [DATA_WIDTH-1:0] in_imag_data,
    //input wire signed qpsk_out_realbit,
    //input wire signed qpsk_out_imagbit,
    //output reg signed [DATA_WIDTH-1:0] out_real_data,
    //output reg signed [DATA_WIDTH-1:0] out_imag_data
);

    input clk;
    input reset;
    input read_ready;
    input [1:0] trans_in;
    
    output reg [DATA_WIDTH-1:0] trans_out;

    //reg[1:0] counter;
    wire[DATA_WIDTH-1:0] symbol_input;
    reg [DATA_WIDTH-1:0] sample_buffer [0:N-1]; //16 bit sample buffer of length 33 bits to hold input samples


    // exported coefficients from MATLAB
    wire [COEFF_WIDTH-1:0] coeffs [0:N-1];//16 bits for each coef since we'll be multiplying them by input later, and 33 registers

    integer i;


    //multiplying by 2^13 moves the binary point 13 places to the left (might need to use <<< idk, be careful w these, 1 adds zeros other is arithmetic)
    //assign in_real_data = {15'b0, qpsk_out_realbit} << 13; //convert 1-bit output from qpsk to Q2.13 format (also known as sfix16_en13 format) aka 16 bits, 1 signed, 2 integer, 13 fractional
    //assign in_imag_data = {15'b0, qpsk_out_imagbit} << 13;
    //assign in_real_data = {{14{1'b0}}, {I}} << 13;
    //assign in_imag_data = {{14{1'b0}}, {Q}} << 13;

    assign symbol_input = {{14{1'b0}}, {trans_in}} << 13;

  

    //coefficients correspond to...
    //filter span in symbols: 8
    //output samples per symbol: 4
    //linear amplitude filter gain: 1
    //use the coefs2 file
    //convert coefficients to Q2.13 format via excel, then enter using 16'sd
    assign coeffs[0] = 16'sd49;
    assign coeffs[1] = 16'sd58;
    assign coeffs[2] =-16'sd7;
    assign coeffs[3] =-16'sd105;
    assign coeffs[4] =-16'sd136;
    assign coeffs[5] =-16'sd26;
    assign coeffs[6] =16'sd184;
    assign coeffs[7] =16'sd331;
    assign coeffs[8] =16'sd232;
    assign coeffs[9] = -16'sd156;
    assign coeffs[10] = -16'sd629;
    assign coeffs[11] = -16'sd798;
    assign coeffs[12] = -16'sd307;
    assign coeffs[13] = 16'sd912;
    assign coeffs[14] = 16'sd2521;
    assign coeffs[15] = 16'sd3893;
    assign coeffs[16] = 16'sd4432;
    assign coeffs[17] = 16'sd3893;
    assign coeffs[18] = 16'sd2521;
    assign coeffs[19] = 16'sd912;
    assign coeffs[20] = -16'sd307;
    assign coeffs[21] = -16'sd798;
    assign coeffs[22] = -16'sd629;
    assign coeffs[23] = -16'sd156;
    assign coeffs[24] = 16'sd232;
    assign coeffs[25] = 16'sd331;
    assign coeffs[26] = 16'sd184;
    assign coeffs[27] = -16'sd26;
    assign coeffs[28] = -16'sd136;
    assign coeffs[29] = -16'sd105;
    assign coeffs[30] = -16'sd7;
    assign coeffs[31] =  16'sd58;
    assign coeffs[32] =  16'sd49;


    //reg signed [DATA_WIDTH-1:0] real_multiplied [0:N-1]; //??? THESE MIGHT NEED TO BE BIGGER, currently 16 bits, maybe make them 32
    //reg signed [DATA_WIDTH-1:0] imag_multiplied [0:N-1];
    //reg signed [DATA_WIDTH+COEFF_WIDTH-1:0] real_multiplied [0:N-1];//33 sample long buffer to hold the multiplications
    //reg signed [DATA_WIDTH+COEFF_WIDTH-1:0] imag_multiplied [0:N-1];
    //reg [32:0] temp_multiplying [0:N-1]; //32 bit reg, 33 of them to store multiplied results before shifting
    wire [DATA_WIDTH-1:0] multiplied [0:N-1]; //16 bit reg, 33 of them to store multiplied results



    parameter empty = 32'd0;
    reg [6:0] counter;


    //instantiate the multiply module to calculate the taps
    multiply m1(sample_buffer[0], coeffs[0], multiplied[0]);
    multiply m2(sample_buffer[1], coeffs[1], multiplied[1]);
    multiply m3(sample_buffer[2], coeffs[2], multiplied[2]);
    multiply m4(sample_buffer[3], coeffs[3], multiplied[3]);
    multiply m5(sample_buffer[4], coeffs[4], multiplied[4]);

    multiply m6(sample_buffer[5], coeffs[5], multiplied[5]);
    multiply m7(sample_buffer[6], coeffs[6], multiplied[6]);
    multiply m8(sample_buffer[7], coeffs[7], multiplied[7]);
    multiply m9(sample_buffer[8], coeffs[8], multiplied[8]);
    multiply m10(sample_buffer[9], coeffs[9], multiplied[9]);

    multiply m11(sample_buffer[10], coeffs[10], multiplied[10]);
    multiply m12(sample_buffer[11], coeffs[11], multiplied[11]);
    multiply m13(sample_buffer[12], coeffs[12], multiplied[12]);
    multiply m14(sample_buffer[13], coeffs[13], multiplied[13]);
    multiply m15(sample_buffer[14], coeffs[14], multiplied[14]);

    multiply m16(sample_buffer[15], coeffs[15], multiplied[15]);
    multiply m17(sample_buffer[16], coeffs[16], multiplied[16]);
    multiply m18(sample_buffer[17], coeffs[17], multiplied[17]);
    multiply m19(sample_buffer[18], coeffs[18], multiplied[18]);
    multiply m20(sample_buffer[19], coeffs[19], multiplied[19]);

    multiply m21(sample_buffer[20], coeffs[20], multiplied[20]);
    multiply m22(sample_buffer[21], coeffs[21], multiplied[21]);
    multiply m23(sample_buffer[22], coeffs[22], multiplied[22]);
    multiply m24(sample_buffer[23], coeffs[23], multiplied[23]);
    multiply m25(sample_buffer[24], coeffs[24], multiplied[24]);

    multiply m26(sample_buffer[25], coeffs[25], multiplied[25]);
    multiply m27(sample_buffer[26], coeffs[26], multiplied[26]);
    multiply m28(sample_buffer[27], coeffs[27], multiplied[27]);
    multiply m29(sample_buffer[28], coeffs[28], multiplied[28]);
    multiply m30(sample_buffer[29], coeffs[29], multiplied[29]);

    multiply m31(sample_buffer[30], coeffs[30], multiplied[30]);
    multiply m32(sample_buffer[31], coeffs[31], multiplied[31]);
    multiply m33(sample_buffer[32], coeffs[32], multiplied[32]);




    // ALWAYS BLOCK FOR SHIFTING SAMPLE REGISTERS
    // ALSO CONTAINS COMBINATIONAL LOGIC FOR ACCUMULATING TAPS
    always @(posedge clk) begin
        if (reset) begin
            trans_out = 16'd0;
            for (i = 0; i < N; i = i + 1) begin //clearing the 33 sized input sample buffer, for loop may be bad way to do this
                sample_buffer[i] <= 16'd0;
            end
        end else begin
            if (read_ready == 1) begin
                sample_buffer[0] <= symbol_input;  //take in the input data at start of buffer
            end else begin
                sample_buffer[0] <= 0; //read in zero instead
            end

            // SHIFT INPUT SAMPLES DOWN IN THE BUFFER (recall buffer has length of # of coefficients)
            for (i = N-1; i > 0; i = i - 1) begin//for i=32; decrement until i=1, runs (creates) 32 times
                sample_buffer[i] <= sample_buffer[i-1];//eg., sample_buffer[1]=sample_buffer[0], sample_buffer[32]=sample_buffer[31]
            end

            trans_out <= multiplied[0] + multiplied[1] + multiplied[2] + multiplied[3] + multiplied[4] + 
                        multiplied[5] + multiplied[6] + multiplied[7] + multiplied[8] + multiplied[9] + 
                        multiplied[10] + multiplied[11] + multiplied[12] + multiplied[13] + multiplied[14] + 
                        multiplied[15] + multiplied[16] + multiplied[17] + multiplied[18] + multiplied[19] + 
                        multiplied[20] + multiplied[21] + multiplied[22] + multiplied[23] + multiplied[24] + 
                        multiplied[25] + multiplied[26] + multiplied[27] + multiplied[28] + multiplied[29] + 
                        multiplied[30] + multiplied[31] + multiplied[32];
        end
    end


    // //PURE COMBINATIONAL LOGIC ALWAYS BLOCK (MULTIPLYING)
    // always @(*) begin
    //     for (i = 0; i < N; i = i + 1) begin //goes from i=0 to i=32, executes 33 times
    //         temp_multiplying[i] <= (sample_buffer[i] * coeffs[i]) >>> 13;
    //         multiplied[i] <= temp_multiplying[i][15:0];// take the bottom 16 bits of temp_multiplying[i] and store them in multiplied[i]
    //     end
    // end


    // //ALWAYS BLOCK FOR ACCUMULATOR (ADDITION) AND REGISTER(GETS PUT BACK TO 0 WHEN COUNTER HITS 33 (32?))
    // always @ (posedge clk, posedge reset) begin
    //     if (reset) begin //???should this just be if (rst | (counter==6'd33))
    //         trans_out <= empty;
    //     end else if (counter == 6'd33) begin//maybe should be if counter == 6d'32?
    //         trans_out <= empty;
    //     end else begin
    //         for (i = 0; i < N; i = i + 1) begin
    //             trans_out <= trans_out + multiplied[i];
    //         end
    //     end
    // end




    // //COUNTER BLOCK
    // //reset the counter from this block if i want to reset it 
    // //note the always block runs when one of these things in the sensitivity list occurs
    // always @ (posedge clk, posedge reset) begin
    //     if (reset)
    //         counter <= 6'd0;
    //     else begin
    //         if (counter == 6'd33) begin
    //             counter <= 6'd0;
    //         end
    //         else begin
    //             counter <= counter + 6'b000001;
    //         end

    //     end
    // end



    // always @(*) begin

    //     //COMPUTE THE FILTER OUTPUT
    //     //either reset accumulator to 0 or collect the output in it depending on control signal
    //     if (control == 1'b0) begin
    //         real_accum = empty; //empty=int2 on diagram
    //         imag_accum = empty;
    //     end
    //     else if (control == 1'b1) begin//this block causing Iteration limit 5000 reached at time 5 ps., when all this adding and multiplying stuff goes i think
    //             for (i = 0; i < N; i = i + 1) begin //goes from i=0 to i=32, executes 33 times
    //                 real_accum <= real_accum + sample_real_buffer[i] * coeffs[i];//start at accum=0+sample_buffer[0]*coeffs[0], next, accum=sample_buffer[0]*coeffs[0], next accum=accum +sample_buffer[1]*coeffs[1]
    //                 imag_accum <= imag_accum + sample_imag_buffer[i] * coeffs[i]; //should i be subtracting something from the accumulator around here, or should it be like a memory element??
    //         end
    //     end

    //     // out_real_data <= real_accum >>> (COEFF_WIDTH);
    //     // out_imag_data <= imag_accum >>> (COEFF_WIDTH);


    // end





endmodule
