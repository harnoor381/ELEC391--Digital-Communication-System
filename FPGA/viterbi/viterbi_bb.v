
module viterbi (
	clk,
	reset,
	sink_val,
	sink_rdy,
	ber_clear,
	eras_sym,
	rr,
	source_val,
	source_rdy,
	numerr,
	normalizations,
	decbit);	

	input		clk;
	input		reset;
	input		sink_val;
	output		sink_rdy;
	input		ber_clear;
	input	[3:0]	eras_sym;
	input	[3:0]	rr;
	output		source_val;
	input		source_rdy;
	output	[7:0]	numerr;
	output	[7:0]	normalizations;
	output		decbit;
endmodule
