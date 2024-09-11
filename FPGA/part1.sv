module part1 (CLOCK_50, CLOCK2_50, KEY, SW, FPGA_I2C_SCLK, FPGA_I2C_SDAT, AUD_XCK, 
		        AUD_DACLRCK, AUD_ADCLRCK, AUD_BCLK, AUD_ADCDAT, AUD_DACDAT, seg);

/////////////////////////
input CLOCK_50, CLOCK2_50;
input [0:0] KEY;
input [9:0] SW;
// I2C Audio/Video config interface
output FPGA_I2C_SCLK;
inout FPGA_I2C_SDAT;
// Audio CODEC
output AUD_XCK;
input AUD_DACLRCK, AUD_ADCLRCK, AUD_BCLK;
input AUD_ADCDAT;
output AUD_DACDAT;
output [6:0] seg [3:0];
// Local wires.
wire read_ready, write_ready;
wire read, write;
wire [15:0] readdata_left, readdata_right;
reg [15:0] writedata_left, writedata_right;
wire [15:0] data_out_clear;
wire reset = ~KEY[0];

/*Wires for pll module*/
wire clk_2MHz, clk_32MHz;

/*Wires for clock divider module*/
wire clk_10KHz;

/*Wires for BER module*/
//wire [31:0] scaled_ber;

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

//-----------------------------
/*
// Instantiate the BER module
    BER ber_inst (
        .clk(clk_32MHz),
        .reset(reset),
        .data_out_clear(data_out_clear),
        .buff_out(buff_out),
        .scaled_ber(scaled_ber)
    );
*/

// Instantiate the SevenSegmentDisplay module
    //SevenSegmentDisplay ssd_inst (.scaled_ber(scaled_ber), .seg(seg));

/*Instantiate Harnoor's parts*/
//internal signals
wire [31:0] enc_bits;
wire [1:0] I;
wire [1:0] Q;
wire [3:0] demod_bits;
wire decbit;
reg [15:0] buff_out;

wire sink_val, sink_rdy, source_val, source_rdy, ber_clear;
wire [7:0] numerr, normalizations;
wire [3:0] eras_sym, rr;

// Set constant values for unused ports
assign sink_val = 1'b1;           // Assuming sink_val is always valid
assign source_rdy = 1'b1;         // Assuming source is always ready
assign ber_clear = 1'b0;          // Assuming no BER clear signal
assign eras_sym = 4'b0000;        // No erasure symbols
//assign rr = 4'b0000;              // default value when using hard decision decoding

con_enc #(16) C0 (clk_32MHz, reset, data_out_clear, enc_bits);
QPSK_mod #(32) M0 (clk_32MHz, reset, enc_bits, I, Q);
QPSK_demod D0 (clk_32MHz, reset, dI, dQ, demod_bits);
buffer b0 (clk_32MHz, reset, decbit, buff_out);

viterbi v0 (
		.clk            (clk_32MHz),            // clk.clk
		.reset          (reset),          // rst.reset
		.sink_val       (sink_val),       //  in.valid
		.sink_rdy       (sink_rdy),       //    .ready
		.ber_clear      (ber_clear),      //    .ber_clear
		.eras_sym       (eras_sym),       //    .eras_sym
		.rr             (demod_bits),             //    .rr
		.source_val     (source_val),     // out.valid
		.source_rdy     (source_rdy),     //    .ready
		.numerr         (numerr),         //    .numerr
		.normalizations (normalizations), //    .normalizations
		.decbit         (decbit)          //    .decbit
	);



/*Instantiate the pll module*/
pll pll_inst (
    .refclk(CLOCK_50),   // Connect the input reference clock
    .rst(reset),          // Connect the reset signal
    .outclk_0(clk_2MHz),  // Output clock at 2 MHz
    .outclk_1(clk_32MHz),  // Output clock at 32 MHz
	.locked(pll_locked)
);

/*Clock divider module*/
clock_divider_10kHz clk10 (clk_2MHz, reset, clk_10KHz);

//---------------

transmitter7 trans_real(.trans_in(I), .trans_out(trans_output_real), .clk(clk_32MHz), .reset(reset), .read_ready(read_ready));
transmitter7 trans_imag(.trans_in(Q), .trans_out(trans_output_imag), .clk(clk_32MHz), .reset(reset), .read_ready(read_ready));

receiver rec_real(.receive_in(chan_output_real), .receive_out(rec_output_real), .clk(clk_32MHz), .reset(reset));
receiver rec_imag(.receive_in(chan_output_imag), .receive_out(rec_output_imag), .clk(clk_32MHz), .reset(reset));

downsampler_b downsamp_real(.in(rec_output_real), .out(downsamp_output_real), .clk(clk_32MHz), .reset(reset));
downsampler_b downsamp_imag(.in(rec_output_imag), .out(downsamp_output_imag), .clk(clk_32MHz), .reset(reset));

assign dQ = downsamp_output_imag [1:0];
assign dI = downsamp_output_real [1:0];

channel chan_instance(.clk(clk_2MHz), .CLOCK_50(clk_32MHz), .real_channel_in(trans_output_real), .imag_channel_in(trans_output_imag), 
						.real_channel_out(chan_output_real), .imag_channel_out(chan_output_imag), .reset(reset));

//----------------


//instantiating downsampler code
downsampler up(.CLOCK_50(clk_2MHz),.reset(reset),.data_in(readdata_right),.down_data_out(data_out_clear));


//assigning read, write, and output statements 
assign read = read_ready && write_ready;
assign write = write_ready && write_ready;
//assign writedata_left = (write && read) ? buff_out: 16'b0;
//assign writedata_right = (write && read) ? buff_out: 16'b0;

always @(posedge CLOCK_50 or posedge reset) begin
	if (reset) begin
		writedata_left <= 16'b0;
		writedata_right <= 16'b0;
		//buff_out <= 16'b0;
	end
	else begin
		if((SW[2]) && (write && read)) begin
			writedata_left <= buff_out; //with noise output
			writedata_right <= buff_out;
		end 
		else begin
			writedata_left <= readdata_left; //clear output 
			writedata_right <= readdata_right;
		end 
	end
end


/////////////////////////////////////////////////////////////////////////////////
// Audio CODEC interface.
//
// The interface consists of the following wires:
// read_ready, write_ready - CODEC ready for read/write operation
// readdata_left, readdata_right - left and right channel data from the CODEC
// read - send data from the CODEC (both channels)
// writedata_left, writedata_right - left and right channel data to the CODEC
// write - send data to the CODEC (both channels)
// AUD_* - should connect to top-level entity I/O of the same name.
//         These signals go directly to the Audio CODEC
// I2C_* - should connect to top-level entity I/O of the same name.
//         These signals go directly to the Audio/Video Config module
/////////////////////////////////////////////////////////////////////////////////
clock_generator my_clock_gen(
// inputs
clk_2MHz, //CLOCK2_50
reset,

// outputs
AUD_XCK
);

audio_and_video_config cfg(
// Inputs
clk_2MHz, //CLOCK_50
reset,

// Bidirectionals
FPGA_I2C_SDAT,
FPGA_I2C_SCLK
);

audio_codec codec(
// Inputs
clk_2MHz, //CLOCK_50
reset,

read, write,
writedata_left, writedata_right,

AUD_ADCDAT,

// Bidirectionals
AUD_BCLK,
AUD_ADCLRCK,
AUD_DACLRCK,

// Outputs
read_ready, write_ready,
readdata_left, readdata_right,
AUD_DACDAT
);


endmodule

module downsampler(
input CLOCK_50,
input wire reset,
input wire [15:0] data_in,
output reg [15:0] down_data_out
);

reg [2:0] count;

always@(posedge CLOCK_50 or posedge reset) begin
if(reset) begin
count<=3'b0;
down_data_out<=16'b0;

end else if (count == 3'b101) begin //taking every 6th sample
down_data_out <= data_in;
count <=3'b0;

end else begin
count <= count +3'b001;
down_data_out <= down_data_out;
end
end
endmodule

module con_enc (CLOCK_256, reset, i2b, enc_bits);

parameter width_i2b = 16;
`define width_enc_bits 2*{width_i2b}
`define K 7                 // Constraint length

input CLOCK_256;
input [width_i2b-1:0] i2b;
input reset;
output reg [`width_enc_bits-1:0] enc_bits;

//wire m, x, y;
//reg [1:0] nextstate, ps_state;

reg [`K-1:0] shift_reg;     // Shift register to hold past input bits
reg [`width_enc_bits-1:0] temp_out;       // Temporary register to hold the output bits
reg [3:0] bit_counter;     // Counter to process each bit of the 16-bit input

// Parameters for the generator polynomials
    reg [`K-1:0] G1 = 7'b1011011; // Generator polynomial 1 (171 in octal)
    reg [`K-1:0] G2 = 7'b1111001; // Generator polynomial 2 (133 in octal)

//Encoding
always @(posedge CLOCK_256 or posedge reset) begin
        if (reset) begin
            bit_counter <= 4'b0000;
            shift_reg <= {`K{1'b0}};
            temp_out <= {`width_enc_bits{1'b0}};
            enc_bits <= {`width_enc_bits{1'b0}};
        end 
        else begin
            if (bit_counter < 4'b1111) begin
                // Shift in the current input bit
                shift_reg <= {shift_reg[`K-2:0], i2b[bit_counter]};

                // Calculate the output bits using the generator polynomials
                temp_out[2*bit_counter]   <= ^(shift_reg & G1); // Output bit 1 (bit wise AND then the XOR reduction to 1 bit)
                temp_out[2*bit_counter+1] <= ^(shift_reg & G2); // Output bit 2 (bit wise AND then the XOR reduction to 1 bit)

                bit_counter <= bit_counter + 4'b0001;

                //enc_bits <= 4'b0000;
            end 
            else begin
                bit_counter <= 4'b0000;
                enc_bits <= temp_out;
            end
        end
    end

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

module buffer (clk, reset, decbit, buff_out);
input clk;
input reset;
input decbit;
output reg [15:0] buff_out;
reg [15:0] temp;

reg [3:0] bit_counter; // Counter to keep track of the number of bits

always @(posedge clk or posedge reset) begin
    if (reset) begin
        buff_out <= {16{1'b0}};
		temp <= {16{1'b0}};
        bit_counter <= 4'b0000;
    end 
	else begin
        // Shift in the decbit and increment the bit counter
        temp <= {temp[14:0], decbit};
        bit_counter <= bit_counter + 4'b0001;
        
        // Reset the bit counter and buffer output after 16 bits
        if (bit_counter == 4'b1111) begin
            bit_counter <= 4'b0000;
			buff_out <= temp;
        end
    end
end
endmodule

module viterbi (
		input  wire       clk,            // clk.clk
		input  wire       sink_val,       //  in.valid
		output wire       sink_rdy,       //    .ready
		input  wire       ber_clear,      //    .ber_clear
		input  wire [3:0] eras_sym,       //    .eras_sym
		input  wire [3:0] rr,             //    .rr
		output wire       source_val,     // out.valid
		input  wire       source_rdy,     //    .ready
		output wire [7:0] numerr,         //    .numerr
		output wire [7:0] normalizations, //    .normalizations
		output wire       decbit,         //    .decbit
		input  wire       reset           // rst.reset
	);

	viterbi_viterbi_ii_0 #(
		.viterbi_type          ("Parallel"),
		.parallel_optimization ("Continuous"),
		.n                     ("4"),
		.ncodes                (1),
		.L                     ("3"),
		.dec_mode              ("V"),
		.ga                    ("7"),
		.gb                    ("6"),
		.gc                    ("5"),
		.gd                    ("4"),
		.ge                    ("0"),
		.gf                    ("0"),
		.gg                    ("0"),
		.acs_units             (1),
		.v                     (32),
		.softbits              (1),
		.rr_size               (4),
		.n_max                 (4),
		.log2_n_max            (2),
		.bmgwide               (7),
		.numerr_size           (8),
		.constraint_length_m_1 (2),
		.vlog_wide             (6),
		.sel_code_size         (1),
		.ber                   ("used"),
		.node_sync             ("unused"),
		.best_state_finder     ("unused"),
		.use_altera_syncram    (0)
	) viterbi_ii_0 (
		.clk             (clk),            // clk.clk
		.reset           (reset),          // rst.reset
		.sink_val        (sink_val),       //  in.valid
		.sink_rdy        (sink_rdy),       //    .ready
		.ber_clear       (ber_clear),      //    .ber_clear
		.eras_sym        (eras_sym),       //    .eras_sym
		.rr              (rr),             //    .rr
		.source_val      (source_val),     // out.valid
		.source_rdy      (source_rdy),     //    .ready
		.numerr          (numerr),         //    .numerr
		.normalizations  (normalizations), //    .normalizations
		.decbit          (decbit),         //    .decbit
		.sink_sop        (1'b0),           // (terminated)
		.sink_eop        (1'b0),           // (terminated)
		.tb_type         (1'b0),           // (terminated)
		.bm_init_state   (2'b00),          // (terminated)
		.bm_init_value   (7'b0000000),     // (terminated)
		.source_sop      (),               // (terminated)
		.source_eop      (),               // (terminated)
		.state_node_sync (2'b00),          // (terminated)
		.sel_code        (1'b0),           // (terminated)
		.tb_length       (6'b000000),      // (terminated)
		.tr_init_state   (2'b00),          // (terminated)
		.bestadd         (),               // (terminated)
		.bestmet         ()                // (terminated)
	);

endmodule

module clock_divider_10kHz(
    input wire clk_2MHz,    // inputing 2MHz clock
    input wire reset,
    output reg clk_10KHz    //outputing the clock to be 10KHz
);


    reg [7:0] count; // 8-bit counter since we need 8 bits to represt the number 200

    always @(posedge clk_2MHz or posedge reset) begin
        if (reset) begin
            count <= 8'b00000000;
            clk_10KHz <= 1'b0;

        end else begin
            if (count == 8'b11000111) begin 
                clk_10KHz <= ~clk_10KHz;
                count <= 8'b00000000;

            end else begin
                count <= count + 8'b00000001; //(incrementing the counteer by 1)
            end
        end
    end

endmodule
/*
module BER (clk, reset, data_out_clear, buff_out, scaled_ber);

	input clk;
    input reset;
    input [15:0] data_out_clear;
    input [15:0] buff_out;
    output reg [31:0] scaled_ber;

	//local wires
    reg [15:0] error_bits;
	reg [31:0] error_count;
    reg [31:0] total_bits;

    // Function to count the number of 1s in a 16-bit vector
    function [4:0] count_ones;
        input [15:0] data;
        begin
            count_ones = data[0] + data[1] + data[2] + data[3] + data[4] + data[5] + data[6] + data[7] +
                         data[8] + data[9] + data[10] + data[11] + data[12] + data[13] + data[14] + data[15];
        end
    endfunction

    // Calculate the number of bit errors 
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            error_count <= 32'b0;
            total_bits <= 32'b0;
            scaled_ber <= 32'b0;
        end else begin
            // XOR the input to the convolutional encoder and output of the Viterbi decoder after the buffer to find the errors
            error_bits = data_out_clear ^ buff_out;
            // Count the number of 1s in the error_bits
            error_count <= error_count + count_ones(error_bits);
            // Increment total bits processed
            total_bits <= total_bits + 16; // Increment by 16 bits processed each clock cycle

            // Calculate BER and scale by 10^5
            if (total_bits > 0) begin
                scaled_ber <= (error_count * 100000) / total_bits;
            end
        end
    end

endmodule
*/
/*
module SevenSegmentDisplay (scaled_ber, seg);

	input [31:0] scaled_ber;
    output reg [6:0] seg [3:0];

    // Internal signals
    reg [3:0] digit [3:0];

    // Seven-segment encoding for digits 0-9
    always @(*) begin
        digit[0] = scaled_ber % 10;
        digit[1] = (scaled_ber / 10) % 10;
        digit[2] = (scaled_ber / 100) % 10;
        digit[3] = (scaled_ber / 1000) % 10;

        seg[0] = encode_digit(digit[0]);
        seg[1] = encode_digit(digit[1]);
        seg[2] = encode_digit(digit[2]);
        seg[3] = encode_digit(digit[3]);
    end

    function [6:0] encode_digit;
        input [3:0] digit;
        case (digit)
            4'd0: encode_digit = 7'b1000000;
            4'd1: encode_digit = 7'b1111001;
            4'd2: encode_digit = 7'b0100100;
            4'd3: encode_digit = 7'b0110000;
            4'd4: encode_digit = 7'b0011001;
            4'd5: encode_digit = 7'b0010010;
            4'd6: encode_digit = 7'b0000010;
            4'd7: encode_digit = 7'b1111000;
            4'd8: encode_digit = 7'b0000000;
            4'd9: encode_digit = 7'b0010000;
            default: encode_digit = 7'b1111111; // Blank or error
        endcase
    endfunction

endmodule
*/

