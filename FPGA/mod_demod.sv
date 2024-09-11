module mod_demod(enc_bits, reset, clk_2MHz, clk_32MHz, clk_10KHz, demod_bits);

input [31:0] enc_bits;
input clk_2MHz;
input clk_32MHz;
input clk_10KHz;
input reset;
output [3:0] demod_bits;

//Wires
//-----transmitter signals---
wire [15:0] trans_output_real;
wire [15:0] trans_output_imag;
wire [15:0] rec_output_real;
wire [15:0] rec_output_imag;
wire [15:0] downsamp_output_real;
wire [15:0] downsamp_output_imag;
wire [1:0] dI;
wire [1:0] dQ;
//channel signals
wire [15:0] chan_output_real;
wire [15:0] chan_output_imag;
//
wire [1:0] I;
wire [1:0] Q;


//Instantiate
QPSK_mod #(32) M0 (clk_32MHz, reset, enc_bits, I, Q);

transmitter7 trans_real(.trans_in(I), .trans_out(trans_output_real), .clk(clk_32MHz), .reset(reset), .read_ready(1'b1));
transmitter7 trans_imag(.trans_in(Q), .trans_out(trans_output_imag), .clk(clk_32MHz), .reset(reset), .read_ready(1'b1));

receiver rec_real(.receive_in(chan_output_real), .receive_out(rec_output_real), .clk(clk_32MHz), .reset(reset));
receiver rec_imag(.receive_in(chan_output_imag), .receive_out(rec_output_imag), .clk(clk_32MHz), .reset(reset));

downsampler_b downsamp_real(.in(rec_output_real), .out(downsamp_output_real), .clk(clk_32MHz), .reset(reset));
downsampler_b downsamp_imag(.in(rec_output_imag), .out(downsamp_output_imag), .clk(clk_32MHz), .reset(reset));

assign dQ = downsamp_output_imag [1:0];
assign dI = downsamp_output_real [1:0];

channel chan_instance(.clk(clk_2MHz), .CLOCK_50(clk_32MHz), .real_channel_in(trans_output_real), .imag_channel_in(trans_output_imag), 
						.real_channel_out(chan_output_real), .imag_channel_out(chan_output_imag), .reset(reset));

QPSK_demod D0 (clk_32MHz, reset, dI, dQ, demod_bits);



endmodule

module QPSK_mod (CLOCK_256, reset, enc_bits, I, Q);

parameter width_enc_bits = 32;
input CLOCK_256;
input reset;
input [width_enc_bits-1:0] enc_bits; // 32-bit input data from convolutional encoder
output reg [1:0] I;    // In-phase component
output reg [1:0] Q;    // Quadrature-phase component

// Internal registers
reg [3:0] bit_counter; // Counter to process each pair of bits from the 32-bit input

// Modulator logic
always @(posedge CLOCK_256 or posedge reset) begin
    if (reset) begin
        bit_counter <= 4'b0000;
        I <= 2'b00;
        Q <= 2'b00;
    end 
    else begin
        if (bit_counter < 4'b1111) begin
            // Extract the current pair of bits from enc_bits
            case (enc_bits[2*bit_counter +: 2]) //it selects 2 bits starting from the bit position 2*bit_counter
                2'b00: begin
                    I <= 2'b01; // cos(0) = 1
                    Q <= 2'b00; // sin(0) = 0
                end
                2'b01: begin
                    I <= 2'b00; // cos(π/2) = 0
                    Q <= 2'b01; // sin(π/2) = 1
                end
                2'b11: begin
                    I <= 2'b11; // cos(π) = -1
                    Q <= 2'b00; // sin(π) = 0
                end
                2'b10: begin
                    I <= 2'b00; // cos(3π/2) = 0
                    Q <= 2'b11; // sin(3π/2) = -1
                end
                default: begin
                    I <= 2'b00;
                    Q <= 2'b00;
                end
            endcase
            bit_counter <= bit_counter + 4'b0001;
        end 
        else begin
            bit_counter <= 1'b0;
        end
    end
end
endmodule

module QPSK_demod (CLOCK_256, reset, I, Q, demod_bits);

input CLOCK_256;
input reset;
input [1:0] I;        // In-phase component
input [1:0] Q;        // Quadrature-phase component
output reg [3:0] demod_bits; // Demodulated output bit

    // Demodulator logic
    always @(posedge CLOCK_256 or posedge reset) begin
        if (reset) begin
            demod_bits <= 4'b0000;
        end 
        else begin
            // Demodulate the I and Q components
            case ({I, Q})
                4'b0100: demod_bits <= 4'b0000; // I = 1, Q = 0
                4'b0001: demod_bits <= 4'b0001; // I = 0, Q = 1
                4'b1100: demod_bits <= 4'b0011; // I = -1, Q = 0
                4'b0011: demod_bits <= 4'b0010; // I = 0, Q = -1
                default: demod_bits <= 4'b0000; // Default case
            endcase
        end
    end
endmodule

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
endmodule

module receiver #(
    parameter N = 33, // Number of filter coefficients
    parameter DATA_WIDTH = 16, // Width of input/output data
    parameter COEFF_WIDTH = 16 // Width of coefficients
) (
    //input wire [1:0] I, //real output bits from qpsk
    //input wire [1:0] Q, //imaginary output bits from qpsk
    receive_in,
    receive_out,
    clk,
    reset
    //read_ready 
    //input wire signed [DATA_WIDTH-1:0] in_real_data,
    //input wire signed [DATA_WIDTH-1:0] in_imag_data,
    //input wire signed qpsk_out_realbit,
    //input wire signed qpsk_out_imagbit,
    //output reg signed [DATA_WIDTH-1:0] out_real_data,
    //output reg signed [DATA_WIDTH-1:0] out_imag_data
);

    input clk;
    input reset;
    //input read_ready;
    
    input [DATA_WIDTH-1:0] receive_in;
    output reg [DATA_WIDTH-1:0] receive_out;


    //reg[1:0] counter;
    //wire[DATA_WIDTH-1:0] receive_in;
    reg [DATA_WIDTH-1:0] sample_buffer [0:N-1]; //16 bit sample buffer of length 33 bits to hold input samples


    // exported coefficients from MATLAB
    wire [COEFF_WIDTH-1:0] coeffs [0:N-1];//16 bits for each coef since we'll be multiplying them by input later, and 33 registers

    integer i;



    //RECIVER COEFFICIENTS (TRANSMITTER COEFFICIENTS DIVIDED BY DECIMATION FACTOR)
    assign coeffs[0] = 16'sd12;
    assign coeffs[1] = 16'sd15;
    assign coeffs[2] = -16'sd2;
    assign coeffs[3] = -16'sd26;
    assign coeffs[4] = -16'sd34;
    assign coeffs[5] = -16'sd6;
    assign coeffs[6] = 16'sd46;
    assign coeffs[7] = 16'sd83;
    assign coeffs[8] = 16'sd58;
    assign coeffs[9] = -16'sd39;
    assign coeffs[10] = -16'sd157;
    assign coeffs[11] = -16'sd200;
    assign coeffs[12] = -16'sd77;
    assign coeffs[13] = 16'sd228;
    assign coeffs[14] = 16'sd630;
    assign coeffs[15] = 16'sd973;
    assign coeffs[16] = 16'sd1108;
    assign coeffs[17] = 16'sd973;
    assign coeffs[18] = 16'sd630;
    assign coeffs[19] = 16'sd228;
    assign coeffs[20] = -16'sd77;
    assign coeffs[21] = -16'sd200;
    assign coeffs[22] = -16'sd157;
    assign coeffs[23] = -16'sd39;
    assign coeffs[24] = 16'sd58;
    assign coeffs[25] = 16'sd83;
    assign coeffs[26] = 16'sd46;
    assign coeffs[27] = -16'sd6;
    assign coeffs[28] = -16'sd34;
    assign coeffs[29] = -16'sd26;
    assign coeffs[30] = -16'sd2;
    assign coeffs[31] = 16'sd15;
    assign coeffs[32] = 16'sd12;


    wire [DATA_WIDTH-1:0] multiplied [0:N-1]; //16 bit reg, 33 of them to store multiplied results



    parameter empty = 32'd0;
    reg [6:0] counter;


    //instantiate the multiply module
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




    // ALWAYS BLOCK FOR SHIFTING REGISTERS
    always @(posedge clk) begin
        if (reset) begin
            receive_out = 16'd0;
            for (i = 0; i < N; i = i + 1) begin //clearing the 33 sized input sample buffer, for loop may be bad way to do this
                sample_buffer[i] <= 16'd0;
            end
        end else begin
            
            sample_buffer[0] <= receive_in;  //take in the input data at start of buffer

            // SHIFT INPUT SAMPLES DOWN IN THE BUFFER (recall buffer has length of # of coefficients)
            for (i = N-1; i > 0; i = i - 1) begin//for i=32; decrement until i=1, runs (creates) 32 times
                sample_buffer[i] <= sample_buffer[i-1];//eg., sample_buffer[1]=sample_buffer[0], sample_buffer[32]=sample_buffer[31]
            end

            receive_out <= multiplied[0] + multiplied[1] + multiplied[2] + multiplied[3] + multiplied[4] + 
                        multiplied[5] + multiplied[6] + multiplied[7] + multiplied[8] + multiplied[9] + 
                        multiplied[10] + multiplied[11] + multiplied[12] + multiplied[13] + multiplied[14] + 
                        multiplied[15] + multiplied[16] + multiplied[17] + multiplied[18] + multiplied[19] + 
                        multiplied[20] + multiplied[21] + multiplied[22] + multiplied[23] + multiplied[24] + 
                        multiplied[25] + multiplied[26] + multiplied[27] + multiplied[28] + multiplied[29] + 
                        multiplied[30] + multiplied[31] + multiplied[32];
        end
    end
endmodule

`define goodstate 1'b1
`define badstate 1'b0

module channel(clk, CLOCK_50, real_channel_in, imag_channel_in, real_channel_out, imag_channel_out, reset);
    input clk, CLOCK_50, reset;
    input [15:0] real_channel_in;
    input [15:0] imag_channel_in;
    output [15:0] imag_channel_out;
    output [15:0] real_channel_out;
    reg [15:0] imag_channel_out;
    reg [15:0] real_channel_out;

    reg present_state;
    wire[15:0] good_chan_awgn;
    wire[15:0] bad_chan_awgn;

    wire[15:0] good_chan_real_awgn;
    wire[15:0] good_chan_imag_awgn;
    wire[15:0] bad_chan_real_awgn;
    wire[15:0] bad_chan_imag_awgn;


    wire [6:0] random;

    //assign the real and imag channels the same noise value
    assign good_chan_imag_awgn = good_chan_awgn;
    assign good_chan_real_awgn = good_chan_awgn;
    assign bad_chan_imag_awgn = bad_chan_awgn;
    assign bad_chan_real_awgn = bad_chan_awgn;


//note parameters from the module being instantiated go outside with the .
//LFSR_7bit rando_gen(.clk(clk), .reset(reset), .random(random));
random_7bit rando_gen(.clk(clk), .rst(reset), .random_num(random));//instantiate random number generator

//instantiate awgn generators

AWGN_21dB_LUT LUT21dB(.clk(clk), .noise_21dB(good_chan_awgn));
AWGN_9dB_LUT LUT9dB(.clk(clk), .noise_9dB(bad_chan_awgn));


        //probly need to use a clk for this that will give it 10kHz frequency
        always @(posedge clk) begin
            if (reset) begin
                present_state <= `goodstate;
                //channel_out = channel_in + good_chan_awgn;
                real_channel_out <= real_channel_in + good_chan_real_awgn;
                imag_channel_out <= imag_channel_in + good_chan_imag_awgn;
        end else begin
            case (present_state)
                `goodstate: present_state  <= (random < 7'd9) ? `badstate: `goodstate;
                `badstate: present_state <= (random < 7'd19) ? `goodstate: `badstate;
                //default: present_state = `goodstate;
            endcase

            case (present_state)
                `goodstate: begin
                     //channel_out = channel_in + good_chan_awgn;
                     real_channel_out <= real_channel_in + good_chan_real_awgn;
                     imag_channel_out <= imag_channel_in + good_chan_imag_awgn;
                end
                `badstate: begin
                    //channel_out = channel_in + bad_chan_awgn;
                    real_channel_out <= real_channel_in + bad_chan_real_awgn;
                    imag_channel_out <= imag_channel_out + bad_chan_imag_awgn;
                end
                default: begin
                    //channel_out = channel_in;
                    real_channel_out <= real_channel_in;
                    imag_channel_out <= imag_channel_in;
                end
            endcase
        end
    end




endmodule

module random_7bit(
    input clk,
    input rst,
    output reg [6:0] random_num = 7'b0111111
);
    reg [6:0] lfsr = 7'b0111111;
    wire feedback;

    // Feedback polynomial: x^7 + x^6 + 1
    //
    assign feedback = lfsr[6] ^ lfsr[5];

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            lfsr <= 7'b0111111; // Initial seed value, ensure it's non-zero
        end else begin
            lfsr <= {lfsr[5:0], feedback};
        end
    end

    always @(posedge clk) begin
        random_num <= lfsr;
    end
endmodule

module downsampler_b (in, out, clk, reset);
  input [15:0] in;
  input clk;
  input reset;
  output reg [15:0] out;

  reg [1:0] counter;

  always @(posedge clk, posedge reset) begin
    if (reset) begin
      counter <= 0;
      out <= 0;
    end else if (counter == 2'b11) begin //decimation of 4
      out <= in;
      counter <= 0;
    end else begin
      counter <= counter + 2'b01; //increment counter

      out <= out;
    end
  end
endmodule

module multiply(in1,in2, out);

    input signed [15:0] in1;//take in two 16 bit signed inputs, symbol and coefficient
    input signed [15:0] in2;

    reg signed [31:0] temp;// temp 32 bit reg to hold the output of the 16 bit multiplication
    output reg signed [15:0] out; //final output variable

    always @(*) begin
        temp = in1*in2 >>> 13; //multiply the inputs and right shift by 13 bits, moves the multiplication to bottom of the reg

        out = temp[15:0]; //take the lower 16 bits of temp and put them into out
    end



endmodule

module AWGN_9dB_LUT (
  input wire clk,
  output reg [15:0] noise_9dB
);
  
    reg [5:0] index;
    reg [15:0] lookup_table_9dB [50:0];


    initial begin

//changed to Q2.13 decimal format in excel, then entered as 16 bit Q2.13 decimals
lookup_table_9dB[0] = 16'sd342;
lookup_table_9dB[1] = 16'sd1849;
lookup_table_9dB[2] = -16'sd4408;
lookup_table_9dB[3] = -16'sd2867;
lookup_table_9dB[4] = 16'sd738;
lookup_table_9dB[5] = -16'sd208;
lookup_table_9dB[6] = 16'sd358;
lookup_table_9dB[7] = 16'sd4252;
lookup_table_9dB[8] = 16'sd2964;
lookup_table_9dB[9] = -16'sd2790;
lookup_table_9dB[10] = 16'sd121;
lookup_table_9dB[11] = -16'sd4070;
lookup_table_9dB[12] = -16'sd636;
lookup_table_9dB[13] = 16'sd744;
lookup_table_9dB[14] = 16'sd783;
lookup_table_9dB[15] = -16'sd1044;
lookup_table_9dB[16] = 16'sd646;
lookup_table_9dB[17] = -16'sd195;
lookup_table_9dB[18] = -16'sd605;
lookup_table_9dB[19] = 16'sd3975;
lookup_table_9dB[20] = 16'sd1320;
lookup_table_9dB[21] = -16'sd1883;
lookup_table_9dB[22] = -16'sd705;
lookup_table_9dB[23] = -16'sd1372;
lookup_table_9dB[24] = 16'sd1213;
lookup_table_9dB[25] = 16'sd1281;
lookup_table_9dB[26] = -16'sd1783;
lookup_table_9dB[27] = -16'sd3330;
lookup_table_9dB[28] = 16'sd3973;
lookup_table_9dB[29] = -16'sd278;
lookup_table_9dB[30] = -16'sd1997;
lookup_table_9dB[31] = -16'sd1153;
lookup_table_9dB[32] = -16'sd3406;
lookup_table_9dB[33] = 16'sd2022;
lookup_table_9dB[34] = -16'sd1168;
lookup_table_9dB[35] = -16'sd12;
lookup_table_9dB[36] = 16'sd2669;
lookup_table_9dB[37] = -16'sd1955;
lookup_table_9dB[38] = 16'sd715;
lookup_table_9dB[39] = 16'sd2943;
lookup_table_9dB[40] = 16'sd83;
lookup_table_9dB[41] = -16'sd2790;
lookup_table_9dB[42] = -16'sd3614;
lookup_table_9dB[43] = -16'sd3163;
lookup_table_9dB[44] = 16'sd1363;
lookup_table_9dB[45] = -16'sd1579;
lookup_table_9dB[46] = 16'sd1155;
lookup_table_9dB[47] = -16'sd422;
lookup_table_9dB[48] = -16'sd5186;
lookup_table_9dB[49] = -16'sd1389;
lookup_table_9dB[50] = 16'sd986;


        index = 6'd0;



    end


    //cycle tthrough the indices of 9dB noise in awgn LUT
    always @(posedge clk) begin

            index <= index + 1;

            if (index >= 50)
                index <= 0;
        
        end


    always @(index) begin
        noise_9dB <= lookup_table_9dB[index];
    end

endmodule

module AWGN_21dB_LUT (
  input wire clk,
  output reg [15:0] noise_21dB
);
  
  reg [5:0] index;
  reg [15:0] lookup_table_21dB [50:0];


    initial begin

    //changed to Q2.13 decimal format in excel, then entered as 16 bit Q2.13 decimals
    lookup_table_21dB[0] = -16'sd773;
    lookup_table_21dB[1] = -16'sd354;
    lookup_table_21dB[2] = -16'sd1574;
    lookup_table_21dB[3] = -16'sd857;
    lookup_table_21dB[4] = 16'sd162;
    lookup_table_21dB[5] = 16'sd1222;
    lookup_table_21dB[6] = -16'sd989;
    lookup_table_21dB[7] = -16'sd438;
    lookup_table_21dB[8] = 16'sd1043;
    lookup_table_21dB[9] = -16'sd657;
    lookup_table_21dB[10] = 16'sd1348;
    lookup_table_21dB[11] = 16'sd309;
    lookup_table_21dB[12] = -16'sd382;
    lookup_table_21dB[13] = -16'sd850;
    lookup_table_21dB[14] = 16'sd589;
    lookup_table_21dB[15] = -16'sd174;
    lookup_table_21dB[16] = 16'sd673;
    lookup_table_21dB[17] = -16'sd797;
    lookup_table_21dB[18] = 16'sd174;
    lookup_table_21dB[19] = -16'sd1405;
    lookup_table_21dB[20] = -16'sd387;
    lookup_table_21dB[21] = -16'sd105;
    lookup_table_21dB[22] = -16'sd1384;
    lookup_table_21dB[23] = 16'sd785;
    lookup_table_21dB[24] = -16'sd468;
    lookup_table_21dB[25] = 16'sd41;
    lookup_table_21dB[26] = -16'sd733;
    lookup_table_21dB[27] = 16'sd1493;
    lookup_table_21dB[28] = 16'sd200;
    lookup_table_21dB[29] = -16'sd481;
    lookup_table_21dB[30] = -16'sd1153;
    lookup_table_21dB[31] = 16'sd474;
    lookup_table_21dB[32] = -16'sd96;
    lookup_table_21dB[33] = -16'sd723;
    lookup_table_21dB[34] = -16'sd145;
    lookup_table_21dB[35] = -16'sd177;
    lookup_table_21dB[36] = -16'sd683;
    lookup_table_21dB[37] = 16'sd513;
    lookup_table_21dB[38] = 16'sd661;
    lookup_table_21dB[39] = 16'sd1100;
    lookup_table_21dB[40] = 16'sd960;
    lookup_table_21dB[41] = -16'sd149;
    lookup_table_21dB[42] = 16'sd272;
    lookup_table_21dB[43] = 16'sd736;
    lookup_table_21dB[44] = -16'sd369;
    lookup_table_21dB[45] = -16'sd270;
    lookup_table_21dB[46] = -16'sd411;
    lookup_table_21dB[47] = 16'sd204;
    lookup_table_21dB[48] = -16'sd2015;
    lookup_table_21dB[49] = 16'sd791;
    lookup_table_21dB[50] = 16'sd373;



    index = 6'd0;

    end


    always @(posedge clk) begin

            index <= index + 1;

            if (index >= 50)
                index <= 0;
        
        end


    always @(index) begin
        noise_21dB <= lookup_table_21dB[index];
    end

endmodule