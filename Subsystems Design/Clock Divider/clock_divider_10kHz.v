//OSR JSGJ
module clock_divider_10kHz(
    input wire clk_2MHz,    // inputing 2MHz clock
    input wire reset,
    output reg clk_10KHz    //outputing the clock to be 10KHz
);


    reg [7:0] count; // 8-bit counter since we need 8 bits to represt the number 200

    always @(posedge clk_2MHz or posedge reset) begin
        if (reset) begin //If reste is asserted, we make the counter = 0 and 10kHz clock to be equal to 0 as well
            count <= 8'b00000000;
            clk_10KHz <= 1'b0;

        end else begin
            if (count == 8'b11000111) begin /*checking if the counter reaches 199 which is 11000111 in binary */
                clk_10KHz <= ~clk_10KHz;
                count <= 8'b00000000; //setting counter = 0 

            end else begin
                count <= count + 8'b00000001; //(incrementing the counter by 1)
            end
        end
    end

endmodule


/* the base code for this module for generated using chatgpt witht the prompt: "Can you please provide a base code for implementing a clock divider in verilog. The code was adjusted to suit to our system's needs and then integrated to be implemented. */
