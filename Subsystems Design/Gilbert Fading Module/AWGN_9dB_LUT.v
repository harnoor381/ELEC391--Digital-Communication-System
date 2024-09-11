



module AWGN_9dB_LUT (
  input wire clk,
  output reg [15:0] noise_9dB
);
  
    reg [5:0] index;
    reg [15:0] lookup_table_9dB [50:0];


    initial begin

//changed to Q2.13 decimal format in excel, then entered as 16 bit Q2.13 decimals
lookup_table_9dB[0] = 16'sd342;
lookup_table_9dB[1] = 16'sd1849;
lookup_table_9dB[2] = -16'sd4408;
lookup_table_9dB[3] = -16'sd2867;
lookup_table_9dB[4] = 16'sd738;
lookup_table_9dB[5] = -16'sd208;
lookup_table_9dB[6] = 16'sd358;
lookup_table_9dB[7] = 16'sd4252;
lookup_table_9dB[8] = 16'sd2964;
lookup_table_9dB[9] = -16'sd2790;
lookup_table_9dB[10] = 16'sd121;
lookup_table_9dB[11] = -16'sd4070;
lookup_table_9dB[12] = -16'sd636;
lookup_table_9dB[13] = 16'sd744;
lookup_table_9dB[14] = 16'sd783;
lookup_table_9dB[15] = -16'sd1044;
lookup_table_9dB[16] = 16'sd646;
lookup_table_9dB[17] = -16'sd195;
lookup_table_9dB[18] = -16'sd605;
lookup_table_9dB[19] = 16'sd3975;
lookup_table_9dB[20] = 16'sd1320;
lookup_table_9dB[21] = -16'sd1883;
lookup_table_9dB[22] = -16'sd705;
lookup_table_9dB[23] = -16'sd1372;
lookup_table_9dB[24] = 16'sd1213;
lookup_table_9dB[25] = 16'sd1281;
lookup_table_9dB[26] = -16'sd1783;
lookup_table_9dB[27] = -16'sd3330;
lookup_table_9dB[28] = 16'sd3973;
lookup_table_9dB[29] = -16'sd278;
lookup_table_9dB[30] = -16'sd1997;
lookup_table_9dB[31] = -16'sd1153;
lookup_table_9dB[32] = -16'sd3406;
lookup_table_9dB[33] = 16'sd2022;
lookup_table_9dB[34] = -16'sd1168;
lookup_table_9dB[35] = -16'sd12;
lookup_table_9dB[36] = 16'sd2669;
lookup_table_9dB[37] = -16'sd1955;
lookup_table_9dB[38] = 16'sd715;
lookup_table_9dB[39] = 16'sd2943;
lookup_table_9dB[40] = 16'sd83;
lookup_table_9dB[41] = -16'sd2790;
lookup_table_9dB[42] = -16'sd3614;
lookup_table_9dB[43] = -16'sd3163;
lookup_table_9dB[44] = 16'sd1363;
lookup_table_9dB[45] = -16'sd1579;
lookup_table_9dB[46] = 16'sd1155;
lookup_table_9dB[47] = -16'sd422;
lookup_table_9dB[48] = -16'sd5186;
lookup_table_9dB[49] = -16'sd1389;
lookup_table_9dB[50] = 16'sd986;


        index = 6'd0;



    end


    //cycle tthrough the indices of 9dB noise in awgn LUT
    always @(posedge clk) begin

            index <= index + 1;

            if (index >= 50)
                index <= 0;
        
        end


    always @(index) begin
        noise_9dB <= lookup_table_9dB[index];
    end





endmodule