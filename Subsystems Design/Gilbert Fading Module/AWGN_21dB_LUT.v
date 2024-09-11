

module AWGN_21dB_LUT (
  input wire clk,
  output reg [15:0] noise_21dB
);
  
  reg [5:0] index;
  reg [15:0] lookup_table_21dB [50:0];


    initial begin

  

    //changed to Q2.13 decimal format in excel, then entered as 16 bit Q2.13 decimals
    lookup_table_21dB[0] = -16'sd773;
    lookup_table_21dB[1] = -16'sd354;
    lookup_table_21dB[2] = -16'sd1574;
    lookup_table_21dB[3] = -16'sd857;
    lookup_table_21dB[4] = 16'sd162;
    lookup_table_21dB[5] = 16'sd1222;
    lookup_table_21dB[6] = -16'sd989;
    lookup_table_21dB[7] = -16'sd438;
    lookup_table_21dB[8] = 16'sd1043;
    lookup_table_21dB[9] = -16'sd657;
    lookup_table_21dB[10] = 16'sd1348;
    lookup_table_21dB[11] = 16'sd309;
    lookup_table_21dB[12] = -16'sd382;
    lookup_table_21dB[13] = -16'sd850;
    lookup_table_21dB[14] = 16'sd589;
    lookup_table_21dB[15] = -16'sd174;
    lookup_table_21dB[16] = 16'sd673;
    lookup_table_21dB[17] = -16'sd797;
    lookup_table_21dB[18] = 16'sd174;
    lookup_table_21dB[19] = -16'sd1405;
    lookup_table_21dB[20] = -16'sd387;
    lookup_table_21dB[21] = -16'sd105;
    lookup_table_21dB[22] = -16'sd1384;
    lookup_table_21dB[23] = 16'sd785;
    lookup_table_21dB[24] = -16'sd468;
    lookup_table_21dB[25] = 16'sd41;
    lookup_table_21dB[26] = -16'sd733;
    lookup_table_21dB[27] = 16'sd1493;
    lookup_table_21dB[28] = 16'sd200;
    lookup_table_21dB[29] = -16'sd481;
    lookup_table_21dB[30] = -16'sd1153;
    lookup_table_21dB[31] = 16'sd474;
    lookup_table_21dB[32] = -16'sd96;
    lookup_table_21dB[33] = -16'sd723;
    lookup_table_21dB[34] = -16'sd145;
    lookup_table_21dB[35] = -16'sd177;
    lookup_table_21dB[36] = -16'sd683;
    lookup_table_21dB[37] = 16'sd513;
    lookup_table_21dB[38] = 16'sd661;
    lookup_table_21dB[39] = 16'sd1100;
    lookup_table_21dB[40] = 16'sd960;
    lookup_table_21dB[41] = -16'sd149;
    lookup_table_21dB[42] = 16'sd272;
    lookup_table_21dB[43] = 16'sd736;
    lookup_table_21dB[44] = -16'sd369;
    lookup_table_21dB[45] = -16'sd270;
    lookup_table_21dB[46] = -16'sd411;
    lookup_table_21dB[47] = 16'sd204;
    lookup_table_21dB[48] = -16'sd2015;
    lookup_table_21dB[49] = 16'sd791;
    lookup_table_21dB[50] = 16'sd373;



    index = 6'd0;

    end


    always @(posedge clk) begin

            index <= index + 1;

            if (index >= 50)
                index <= 0;
        
        end


    always @(index) begin
        noise_21dB <= lookup_table_21dB[index];
    end




endmodule