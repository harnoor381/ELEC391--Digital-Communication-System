`timescale 1 ps / 1 ps
  module viterbi_viterbi_ii_0 (
    input  [1:0] state_node_sync,
    input  [5:0] tb_length,
    input  [1:0] tr_init_state,
    output [1:0] bestadd,
    output [6:0] bestmet,
    input  [1:0] bm_init_state,
    input  [6:0] bm_init_value,
    input  [3:0] eras_sym,
    input  [3:0] rr,
    output [7:0] numerr,
    output [7:0] normalizations,
    input        sink_eop,
    input        reset,
    output       sink_rdy,
    input        tb_type,
    input        ber_clear,
    input        clk,
    output       source_sop,
    output       decbit,
    output       source_val,
    output       source_eop,
    input        sel_code,
    input        sink_sop,
    input        source_rdy,
    input        sink_val);

    parameter viterbi_type          = "Parallel";
    parameter parallel_optimization = "Continuous";
    parameter n                     = "4";
    parameter ncodes                = 1;
    parameter L                     = "3";
    parameter dec_mode              = "V";
    parameter ga                    = "7";
    parameter gb                    = "6";
    parameter gc                    = "5";
    parameter gd                    = "4";
    parameter ge                    = "0";
    parameter gf                    = "0";
    parameter gg                    = "0";
    parameter acs_units             = 1;
    parameter v                     = 32;
    parameter softbits              = 1;
    parameter rr_size               = 4;
    parameter n_max                 = 4;
    parameter log2_n_max            = 2;
    parameter bmgwide               = 7;
    parameter numerr_size           = 8;
    parameter constraint_length_m_1 = 2;
    parameter vlog_wide             = 6;
    parameter sel_code_size         = 1;
    parameter ber                   = "used";
    parameter node_sync             = "unused";
    parameter best_state_finder     = "unused";
    parameter use_altera_syncram    = 0;

auk_vit_top #(
    .viterbi_type          (viterbi_type),
    .parallel_optimization (parallel_optimization),
    .n                     (n),
    .ncodes                (ncodes),
    .L                     (L),
    .dec_mode              (dec_mode),
    .ga                    (ga),
    .gb                    (gb),
    .gc                    (gc),
    .gd                    (gd),
    .ge                    (ge),
    .gf                    (gf),
    .gg                    (gg),
    .acs_units             (acs_units),
    .v                     (v),
    .softbits              (softbits),
    .rr_size               (rr_size),
    .n_max                 (n_max),
    .log2_n_max            (log2_n_max),
    .bmgwide               (bmgwide),
    .numerr_size           (numerr_size),
    .constraint_length_m_1 (constraint_length_m_1),
    .vlog_wide             (vlog_wide),
    .sel_code_size         (sel_code_size),
    .ber                   (ber),
    .node_sync             (node_sync),
    .best_state_finder     (best_state_finder),
    .use_altera_syncram    (use_altera_syncram),
    .ini_filename          ("viterbi_viterbi_ii_0_ini.hex")
) auk_vit_top (
    .state_node_sync (state_node_sync),
    .tb_length       (tb_length),
    .tr_init_state   (tr_init_state),
    .bestadd         (bestadd),
    .bestmet         (bestmet),
    .bm_init_state   (bm_init_state),
    .bm_init_value   (bm_init_value),
    .eras_sym        (eras_sym),
    .rr              (rr),
    .numerr          (numerr),
    .normalizations  (normalizations),
    .sink_eop        (sink_eop),
    .reset           (reset),
    .sink_rdy        (sink_rdy),
    .tb_type         (tb_type),
    .ber_clear       (ber_clear),
    .clk             (clk),
    .source_sop      (source_sop),
    .decbit          (decbit),
    .source_val      (source_val),
    .source_eop      (source_eop),
    .sel_code        (sel_code),
    .sink_sop        (sink_sop),
    .source_rdy      (source_rdy),
    .sink_val        (sink_val)
);

endmodule
