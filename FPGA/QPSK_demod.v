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