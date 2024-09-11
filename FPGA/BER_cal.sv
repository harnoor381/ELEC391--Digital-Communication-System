module BER (
    input clk,
    input reset,
    input [15:0] data_out_clear,
    input [15:0] buff_out,
    output reg [31:0] error_count,
    output reg [31:0] total_bits,
    output reg [31:0] scaled_ber
);

    reg [15:0] error_bits;

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

module SevenSegmentDisplay (
    input [31:0] scaled_ber,
    output reg [6:0] seg [3:0]
);

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