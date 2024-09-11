module downsampler(in, out, clk, reset);
  input [15:0] in;
  input clk;
  input reset;
  output reg [15:0] out;

  reg [1:0] counter;

  always @(posedge clk, posedge reset) begin
    if (reset) begin
      counter <= 0;
      out <= 0;
    end else if (counter == 3) begin //decimation of 4
      out <= in;
      counter <= 0;
    end else begin
      counter <= counter + 1; //increment counter

      out <= out;
    end
  end
endmodule