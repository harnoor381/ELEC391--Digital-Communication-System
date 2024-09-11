//OSR JSGJ

//Declaring Module 
module downsampler(
    input CLOCK_50,
    input wire reset,
    input wire [15:0] data_in,
    output reg [15:0] down_data_out
);
    reg [2:0] count; //declaring a counter

    always @(posedge CLOCK_50 or posedge reset) begin

       if (reset) begin //if reset then initialising the outputs with count = 0, down_data_out = 0
           count <= 3'b0;
           down_data_out <= 16'b0;
        end else if (count == 3'b101) begin  //if counter's value reaches 5, then we take the down_data_out equal to the input data
            down_data_out <= data_in;
             count <= 3'b0;
         end else begin //if none of the above conditions are satisfied, we increment the counter by 1 and keep down_data_out as it is
           count <= count + 3'b001;
	down_data_out <= down_data_out;
       end
    end

endmodule

//The pseudo code for the downsampler was generated using chatgpt with the prompt: "Please provide a basic code for downsampler in Verilog"
//Necessary changes to the overall code, the factors, and the paramters was made by me