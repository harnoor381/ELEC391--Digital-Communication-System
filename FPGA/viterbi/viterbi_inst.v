	viterbi u0 (
		.clk            (<connected-to-clk>),            // clk.clk
		.reset          (<connected-to-reset>),          // rst.reset
		.sink_val       (<connected-to-sink_val>),       //  in.valid
		.sink_rdy       (<connected-to-sink_rdy>),       //    .ready
		.ber_clear      (<connected-to-ber_clear>),      //    .ber_clear
		.eras_sym       (<connected-to-eras_sym>),       //    .eras_sym
		.rr             (<connected-to-rr>),             //    .rr
		.source_val     (<connected-to-source_val>),     // out.valid
		.source_rdy     (<connected-to-source_rdy>),     //    .ready
		.numerr         (<connected-to-numerr>),         //    .numerr
		.normalizations (<connected-to-normalizations>), //    .normalizations
		.decbit         (<connected-to-decbit>)          //    .decbit
	);

