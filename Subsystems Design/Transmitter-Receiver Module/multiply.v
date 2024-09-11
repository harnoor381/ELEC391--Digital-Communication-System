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