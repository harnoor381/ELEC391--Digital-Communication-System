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