

//`timescale 1ps/1ps


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


//question, ask prof if i need to be using nonblocking assignments for outputs of my state machine here
    // always @(posedge clk) begin
    //     if (reset) begin
    //         present_state <= `goodstate;
    //         channel_out <= channel_in + good_chan_awgn;
    //     end else begin
    //         case (present_state)
    //             `goodstate: present_state <= (random < 7'd9) ? `badstate: `goodstate;
    //             `badstate: present_state <= (random < 7'd19) ? `goodstate: `badstate;
    //             //default: present_state <= `goodstate;
    //         endcase

    //         case (present_state)
    //             `goodstate: channel_out <= channel_in + good_chan_awgn;
    //             `badstate: channel_out <= channel_in + bad_chan_awgn;
    //             default: channel_out <= channel_in;
    //         endcase
    //     end
    // end


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



// module random_int_gen(clk, rst_n, data);
//     input clk;
//     input rst_n;
//     output reg [6:0] data;

//     wire feedback = data[6] ^ data[0];

//     always @(posedge clk or negedge rst_n)
//     if (~rst_n) 
//         data <= 6'd63;
//     else
//         data <= {data[6:0], feedback} ;

// endmodule



// module LFSR_7bit (
//     clk,         // Clock input
//     reset,       // Reset input
//     random // 7-bit random output
// );
//     input clk;
//     input reset;
//     output wire [6:0] random;

//     wire feedback;


//     reg [6:0] lfsr_reg; // 7-bit register for LFSR

//     // Feedback polynomial for 7-bit LFSR: x^7 + x^6 + 1
//     assign feedback = lfsr_reg[6] ^ lfsr_reg[5];

//     always @(posedge clk or posedge reset) begin
//         if (reset) begin
//             lfsr_reg <= 7'b0000001; // Initial seed value
//         end else begin
//             lfsr_reg <= {lfsr_reg[5:0], feedback}; // Shift and feedback
//         end
//     end

//     assign random = lfsr_reg;
// endmodule


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
