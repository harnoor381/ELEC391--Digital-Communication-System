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