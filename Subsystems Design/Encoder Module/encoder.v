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