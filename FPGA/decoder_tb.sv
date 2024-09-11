module viterbi_tb;

    // Signals
    reg clk;
    reg sink_val;
    wire sink_rdy;
    reg ber_clear;
    reg [3:0] eras_sym;
    reg [3:0] rr;
    wire source_val;
    reg source_rdy;
    wire [7:0] numerr;
    wire [7:0] normalizations;
    wire decbit;
    reg reset;

    // Instantiate the viterbi module
    viterbi DUT (
        .clk(clk),
        .sink_val(sink_val),
        .sink_rdy(sink_rdy),
        .ber_clear(ber_clear),
        .eras_sym(eras_sym),
        .rr(rr),
        .source_val(source_val),
        .source_rdy(source_rdy),
        .numerr(numerr),
        .normalizations(normalizations),
        .decbit(decbit),
        .reset(reset)
    );

    // Clock generation
    initial begin
        clk = 0;
        forever #5 clk = ~clk; // 10 time units period
    end

    // Test sequence
    initial begin
        // Apply reset
        reset = 1;
        sink_val = 0;
        ber_clear = 0;
        eras_sym = 4'b0;
        rr = 4'b0;
        source_rdy = 0;
        #20;
        reset = 0;

        // Random test cases
        repeat (10) begin
            // Generate random 4-bit input for rr
            rr = $random % 16; // Random 4-bit value
            eras_sym = $random % 16; // Random 4-bit value for eras_sym
            
            sink_val = 1;
            #10;
            source_rdy = 1;
            #10;
            if (source_val) begin
                $display("Random test case: rr = %b, eras_sym = %b, numerr = %d, normalizations = %d, decbit = %b", rr, eras_sym, numerr, normalizations, decbit);
            end
            sink_val = 0;
            source_rdy = 0;
            #10;
        end

        // End of test
        #100;
        $stop;
    end

endmodule
