-- (C) 2001-2018 Intel Corporation. All rights reserved.
-- Your use of Intel Corporation's design tools, logic functions and other 
-- software and tools, and its AMPP partner logic functions, and any output 
-- files from any of the foregoing (including device programming or simulation 
-- files), and any associated documentation or information are expressly subject 
-- to the terms and conditions of the Intel Program License Subscription 
-- Agreement, Intel FPGA IP License Agreement, or other applicable 
-- license agreement, including, without limitation, that your use is for the 
-- sole purpose of programming logic devices manufactured by Intel and sold by 
-- Intel or its authorized distributors.  Please refer to the applicable 
-- agreement for further details.


-------------------------------------------------------------------------
-------------------------------------------------------------------------
--
-- Revision Control Information
--
-- $Workfile:   auk_vit_hyb_top_atl_arc_rtl.vhd  $
-- $Archive:   Y:/IP_PVCS/archives/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_top_atl_arc_rtl.vhd-arc  $
--
-- $RCSfile: auk_vit_hyb_top_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_top_atl_arc_rtl.vhd,v $
--
-- $Revision: #1 $
-- $Date: 2008/07/12 $
-- Check in by           : $Author: max $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description    :  
--
-- ALTERA Confidential and Proprietary
-- Copyright 2002 (c) Altera Corporation
-- All rights reserved
--
-------------------------------------------------------------------------
-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


Entity auk_vit_top is
    Generic (
        viterbi_type        : STRING  := "Parallel";
        n                     : STRING  := "2";
        L                     : STRING  := "9"; 
        dec_mode            : STRING  := "V"; 
        ncodes                 : NATURAL := 1; 
        n_max                 : NATURAL := 2;
        log2_n_max          : NATURAL := 1;
        rr_size                : NATURAL := 8;
        constraint_length_m_1 : NATURAL := 8; 
        v                     : NATURAL := 54;
        acs_units             : NATURAL := 8;
        softbits             : NATURAL := 4;
        bmgwide             : NATURAL := 9; 
        vlog_wide           : NATURAL := 6;
        sel_code_size       : NATURAL := 1;
        numerr_size         : NATURAL := 12; 
        size_bus            : NATURAL := 16;
        ga                     : STRING := "369"; 
        gb                     : STRING := "491"; 
        gc                     : STRING := "0"; 
        gd                     : STRING := "0"; 
        ge                     : STRING := "0"; 
        gf                     : STRING := "0"; 
        gg                     : STRING := "0";
        ber                 : STRING := "unused";
        parallel_optimization : STRING := "None";
        best_state_finder   : STRING := "used";
        sm_init_logic       : STRING := "used";
        ini_filename        : STRING := "filename.hex";
        use_altera_syncram  : NATURAL := 1;
        node_sync             : STRING := "unused"
    );
    Port (
        
        clk                 : in std_logic;
        reset               : in Std_Logic;
        ber_clear           : in Std_Logic;
        
        -- These are the Sink side control signal
        sink_rdy            : out Std_Logic; 
        sink_val            : in  Std_Logic; 
        sink_sop            : in  Std_Logic; 
        sink_eop            : in  Std_Logic; 

        rr                  : in Std_Logic_Vector(rr_size downto 1);
        eras_sym            : in Std_Logic_Vector(n_max downto 1);
        state_node_sync     : in Std_Logic_Vector(log2_n_max downto 1);
        sel_code            : in Std_Logic_Vector(sel_code_size downto 1);
        tr_init_state       : in Std_Logic_Vector(constraint_length_m_1 downto 1);
        tb_type             : in Std_Logic;
        bm_init_state       : in Std_Logic_Vector(constraint_length_m_1 downto 1);
        bm_init_value       : in Std_Logic_Vector(bmgwide downto 1);
        tb_length           : in Std_Logic_Vector(vlog_wide downto 1);
        normalizations      : out Std_Logic_Vector(8 downto 1);
        

        -- Slave source side control signals
        source_rdy          : in Std_Logic; 
        source_val          : out Std_Logic; 
        source_sop          : out Std_Logic; 
        source_eop          : out Std_Logic; 

        decbit              : out Std_Logic;
        bestmet             : out Std_Logic_Vector(bmgwide downto 1);
        bestadd             : out Std_Logic_Vector(constraint_length_m_1 downto 1);
         numerr              : out Std_Logic_Vector(numerr_size downto 1)
    );    
end entity auk_vit_top;    


Architecture rtl of auk_vit_top is


    COMPONENT auk_vit_par_top_atl
        Generic (
                n                     : STRING  := "2"; 
                L                     : STRING  := "7"; 
                dec_mode            : STRING  := "V"; 
                ncodes                 : NATURAL := 1; 
                n_max                 : NATURAL := 2; 
                log2_n_max          : NATURAL := 1;
                rr_size                : NATURAL := 8; 
                constraint_length_m_1 : NATURAL := 8; 
                v                     : NATURAL := 54; 
                softbits             : NATURAL := 4;
                bmgwide             : NATURAL := 9;
                vlog_wide           : NATURAL := 6;
                sel_code_size       : NATURAL := 1;
                numerr_size         : NATURAL := 12; 
                size_bus            : NATURAL := 16; 
                ga                     : STRING  := "91 91";
                gb                     : STRING  := "121 101";
                gc                     : STRING  := "0 125";
                gd                     : STRING  := "0 0";
                ge                     : STRING  := "0 0";
                gf                     : STRING  := "0 0";
                gg                     : STRING  := "0 0";
                parallel_optimization : STRING := "None"; 
                best_state_finder   : STRING  := "used";
                ber                 : STRING  := "used";
                ini_filename        : STRING  := "filename.hex";
                use_altera_syncram  : NATURAL := 1;
                node_sync             : STRING  := "unused"
        );
        Port (
                clk                 : in  std_logic; 
                reset               : in  Std_Logic;
                sink_rdy            : out Std_Logic; 
                sink_val            : in  Std_Logic; 
                sink_sop            : in  Std_Logic; 
                sink_eop            : in  Std_Logic; 
                rr                  : in  Std_Logic_Vector(rr_size downto 1);
                eras_sym            : in  Std_Logic_Vector(n_max downto 1);
                state_node_sync     : in  Std_Logic_Vector(log2_n_max downto 1);
                sel_code            : in  Std_Logic_Vector(sel_code_size downto 1);
                tr_init_state       : in  Std_Logic_Vector(constraint_length_m_1 downto 1);
                tb_type             : in  Std_Logic;
                tb_length           : in  Std_Logic_Vector(vlog_wide downto 1);
                ber_clear           : in  std_logic;
                source_rdy          : in  Std_Logic; 
                source_val          : out Std_Logic; 
                source_sop          : out Std_Logic; 
                source_eop          : out Std_Logic; 
                decbit              : out Std_Logic;
                bestmet             : out Std_Logic_Vector(bmgwide downto 1);
                bestadd             : out Std_Logic_Vector(constraint_length_m_1 downto 1);
                normalizations      : out Std_Logic_Vector(8 downto 1);
                numerr              : out Std_Logic_Vector(numerr_size downto 1)
        );
    END COMPONENT;

    COMPONENT auk_vit_hyb_top_atl
        Generic (
                n                     : STRING  := "2"; 
                L                     : STRING  := "9"; 
                dec_mode              : STRING  := "V";
                ncodes                : NATURAL := 1; 
                n_max                 : NATURAL := 2;
                log2_n_max            : NATURAL := 1;
                rr_size               : NATURAL := 8; 
                constraint_length_m_1 : NATURAL := 8;
                v                     : NATURAL := 54; 
                acs_units             : NATURAL := 8; 
                softbits              : NATURAL := 4;
                bmgwide               : NATURAL := 9; 
                vlog_wide             : NATURAL := 6;
                sel_code_size         : NATURAL := 1;
                numerr_size           : NATURAL := 12; 
                size_bus              : NATURAL := 16;
                ga                     : STRING  := "369";
                gb                     : STRING  := "491";
                gc                     : STRING  := "0";
                gd                     : STRING  := "0"; 
                ge                     : STRING  := "0"; 
                gf                     : STRING  := "0"; 
                gg                     : STRING  := "0";
                BER                 : STRING  := "unused";
                sm_init_logic       : STRING  := "used";
                use_altera_syncram  : NATURAL := 1;
                node_sync             : STRING  := "unused"
        );
        Port (
                clk                 : in  std_logic;
                reset               : in  Std_Logic;
                ber_clear           : in  std_logic;
                sink_rdy            : out Std_Logic; 
                sink_val            : in  Std_Logic; 
                sink_sop            : in  Std_Logic; 
                sink_eop            : in  Std_Logic; 
                rr                  : in  Std_Logic_Vector(rr_size downto 1);
                eras_sym            : in  Std_Logic_Vector(n_max downto 1);
                state_node_sync     : in  Std_Logic_Vector(log2_n_max downto 1);
                sel_code            : in  Std_Logic_Vector(sel_code_size downto 1);
                tr_init_state       : in  Std_Logic_Vector(constraint_length_m_1 downto 1);
                bm_init_state       : in  Std_Logic_Vector(constraint_length_m_1 downto 1);
                bm_init_value       : in  Std_Logic_Vector(bmgwide downto 1);
                tb_length           : in  Std_Logic_Vector(vlog_wide downto 1);
                normalizations      : out Std_Logic_Vector(8 downto 1);
                source_rdy          : in  Std_Logic; 
                source_val          : out Std_Logic; 
                source_sop          : out Std_Logic; 
                source_eop          : out Std_Logic; 
                decbit              : out Std_Logic;
                bestmet             : out Std_Logic_Vector(bmgwide downto 1);
                bestadd             : out Std_Logic_Vector(constraint_length_m_1 downto 1);
                numerr              : out Std_Logic_Vector(numerr_size downto 1)
        );    
    END COMPONENT;




    begin


    is_parallel_decoder: if viterbi_type="Parallel" generate
    
        parallel_decoder : auk_vit_par_top_atl
            Generic map (n => n, L => L, dec_mode => dec_mode, ncodes => ncodes,n_max => n_max, log2_n_max => log2_n_max, rr_size => rr_size,
                         constraint_length_m_1 => constraint_length_m_1, v => v, softbits => softbits, bmgwide => bmgwide, 
                         vlog_wide => vlog_wide, sel_code_size => sel_code_size, numerr_size => numerr_size, size_bus => size_bus,
                         ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg, parallel_optimization => parallel_optimization,
                         best_state_finder => best_state_finder, ini_filename => ini_filename, use_altera_syncram => use_altera_syncram,
                         ber => ber, node_sync => node_sync)
            Port map (
                clk => clk, reset => reset, sink_rdy => sink_rdy, sink_val => sink_val, sink_sop => sink_sop,
                sink_eop => sink_eop, rr => rr, eras_sym => eras_sym, state_node_sync => state_node_sync,
                sel_code => sel_code, tr_init_state => tr_init_state, tb_type => tb_type, tb_length => tb_length, 
                ber_clear => ber_clear, source_rdy => source_rdy, source_val => source_val, source_sop => source_sop,
                source_eop => source_eop, decbit => decbit, bestmet => bestmet, bestadd => bestadd, normalizations => normalizations,
                numerr => numerr);

    end generate is_parallel_decoder;


    is_hybrid_decoder: if viterbi_type="Hybrid" generate

        hybrid_decoder :  auk_vit_hyb_top_atl
            Generic map (n => n, L => L, dec_mode => dec_mode, ncodes => ncodes, n_max => n_max, log2_n_max => log2_n_max, rr_size => rr_size,
                         constraint_length_m_1 => constraint_length_m_1, v => v, acs_units => acs_units, softbits => softbits,
                         bmgwide => bmgwide, vlog_wide => vlog_wide, sel_code_size => sel_code_size,numerr_size => numerr_size,
                         size_bus => size_bus, ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg, BER => ber,
                         sm_init_logic => sm_init_logic, node_sync => node_sync, use_altera_syncram => use_altera_syncram)
            Port map (
                clk => clk, reset => reset, sink_rdy => sink_rdy, sink_val => sink_val, sink_sop => sink_sop,
                sink_eop => sink_eop, rr => rr, eras_sym => eras_sym, state_node_sync => state_node_sync,
                sel_code => sel_code, tr_init_state => tr_init_state, bm_init_state => bm_init_state, bm_init_value => bm_init_value, 
                tb_length => tb_length, normalizations => normalizations, ber_clear => ber_clear, source_rdy => source_rdy, source_val => source_val, source_sop => source_sop,
                source_eop => source_eop, decbit => decbit, bestmet => bestmet, bestadd => bestadd, numerr => numerr);

    end generate is_hybrid_decoder;
    
end architecture rtl;    
