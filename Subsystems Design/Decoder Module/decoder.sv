module viterbi (
		input  wire       clk,            // clk.clk
		input  wire       sink_val,       //  in.valid
		output wire       sink_rdy,       //    .ready
		input  wire       ber_clear,      //    .ber_clear
		input  wire [3:0] eras_sym,       //    .eras_sym
		input  wire [3:0] rr,             //    .rr
		output wire       source_val,     // out.valid
		input  wire       source_rdy,     //    .ready
		output wire [7:0] numerr,         //    .numerr
		output wire [7:0] normalizations, //    .normalizations
		output wire       decbit,         //    .decbit
		input  wire       reset           // rst.reset
	);

	viterbi_viterbi_ii_0 #(
		.viterbi_type          ("Parallel"),
		.parallel_optimization ("Continuous"),
		.n                     ("4"),
		.ncodes                (1),
		.L                     ("3"),
		.dec_mode              ("V"),
		.ga                    ("7"),
		.gb                    ("6"),
		.gc                    ("5"),
		.gd                    ("4"),
		.ge                    ("0"),
		.gf                    ("0"),
		.gg                    ("0"),
		.acs_units             (1),
		.v                     (32),
		.softbits              (1),
		.rr_size               (4),
		.n_max                 (4),
		.log2_n_max            (2),
		.bmgwide               (7),
		.numerr_size           (8),
		.constraint_length_m_1 (2),
		.vlog_wide             (6),
		.sel_code_size         (1),
		.ber                   ("used"),
		.node_sync             ("unused"),
		.best_state_finder     ("unused"),
		.use_altera_syncram    (0)
	) viterbi_ii_0 (
		.clk             (clk),            // clk.clk
		.reset           (reset),          // rst.reset
		.sink_val        (sink_val),       //  in.valid
		.sink_rdy        (sink_rdy),       //    .ready
		.ber_clear       (ber_clear),      //    .ber_clear
		.eras_sym        (eras_sym),       //    .eras_sym
		.rr              (rr),             //    .rr
		.source_val      (source_val),     // out.valid
		.source_rdy      (source_rdy),     //    .ready
		.numerr          (numerr),         //    .numerr
		.normalizations  (normalizations), //    .normalizations
		.decbit          (decbit),         //    .decbit
		.sink_sop        (1'b0),           // (terminated)
		.sink_eop        (1'b0),           // (terminated)
		.tb_type         (1'b0),           // (terminated)
		.bm_init_state   (2'b00),          // (terminated)
		.bm_init_value   (7'b0000000),     // (terminated)
		.source_sop      (),               // (terminated)
		.source_eop      (),               // (terminated)
		.state_node_sync (2'b00),          // (terminated)
		.sel_code        (1'b0),           // (terminated)
		.tb_length       (6'b000000),      // (terminated)
		.tr_init_state   (2'b00),          // (terminated)
		.bestadd         (),               // (terminated)
		.bestmet         ()                // (terminated)
	);

endmodule