module mod_demod_tb;

    // Inputs
    reg [31:0] enc_bits;
    reg clk_2MHz;
    reg clk_32MHz;
    reg clk_10KHz;
    reg reset;

    // Outputs
    wire [3:0] demod_bits;

    // Instantiate the Unit Under Test (UUT)
    mod_demod uut (
        .enc_bits(enc_bits), 
        .clk_2MHz(clk_2MHz), 
        .clk_32MHz(clk_32MHz), 
        .clk_10KHz(clk_10KHz), 
        .reset(reset), 
        .demod_bits(demod_bits)
    );

    // Clock generation
    initial begin
        clk_2MHz = 0;
        forever #250 clk_2MHz = ~clk_2MHz; // 2 MHz clock
    end

    initial begin
        clk_32MHz = 0;
        forever #15.625 clk_32MHz = ~clk_32MHz; // 32 MHz clock
    end

    initial begin
        clk_10KHz = 0;
        forever #50000 clk_10KHz = ~clk_10KHz; // 10 KHz clock
    end

    // Test sequence
    initial begin
        // Initialize Inputs
        #500;
        reset = 1;
        //clk_10KHz = 1;
        enc_bits = 0;
        #500;

        // Deassert reset
        reset = 0;
        //clk_10KHz = 1;
        #500;

        // Apply random test cases
        repeat (10) @(posedge clk_2MHz) begin
            enc_bits = $random; // Random 32-bit value
            //clk_10KHz = 1;
            #500; // Wait for 32 MHz clock cycles
        end

        // Add additional test cases if needed

        // End of test
        #1000;
        $stop;
    end

endmodule
