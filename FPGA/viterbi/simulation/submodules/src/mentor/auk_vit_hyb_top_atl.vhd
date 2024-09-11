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
use work.vi_interface.all;
use work.vi_functions.all;


Entity auk_vit_hyb_top_atl is
    Generic (
                n                     : STRING  := "2"; --"2_2_3_4";
                L                     : STRING  := "9"; --"3_5_5_5";
                dec_mode        : STRING  := "V"; --"V_V_V_T";
                ncodes             : NATURAL := 1; --4;
                n_max             : NATURAL := 2; --4;
                log2_n_max  : NATURAL := 1;
                rr_size            : NATURAL := 8; --16;
                constraint_length_m_1 : NATURAL := 8; --4;
                v                     : NATURAL := 54; --30;
                acs_units     : NATURAL := 8; --1;
                 softbits         : NATURAL := 4;
                bmgwide         : NATURAL := 9; --10;
                vlog_wide : NATURAL := 6;
                sel_code_size : NATURAL := 1;
                numerr_size  : NATURAL := 12; -- depens on how big the block is going to be
                size_bus    : NATURAL := 16; -- 16 or 32
                ga                     : STRING := "369"; --"5_19_21_21";
                gb                     : STRING := "491"; --"7_29_27_23";
                gc                     : STRING := "0"; --"0_0_31_27";
                gd                     : STRING := "0"; --"0_0_0_31";
                ge                     : STRING := "0"; --"0_0_0_0";
                gf                     : STRING := "0"; --"0_0_0_0";
                gg                     : STRING := "0"; --"0_0_0_0";
                BER                 : STRING := "unused";
                sm_init_logic : STRING := "used";
                use_altera_syncram  : NATURAL := 1;
                node_sync     : STRING := "unused"
    );
    Port (
        -- these signals stay for atlantic, clk maybe to become clock or clk
        clk   : in std_logic;
        reset : in Std_Logic;
        ber_clear : in std_logic;
        -- These are the Sink side control signal
        sink_rdy        : out Std_Logic; 
        sink_val        : in  Std_Logic; 
        sink_sop        : in  Std_Logic; 
        sink_eop        : in  Std_Logic; 

        -- rr and eras_sym are data input. How are they going to be merged for SOPC
        -- I'll think about that later
        rr : in Std_Logic_Vector(rr_size downto 1);
        eras_sym : in Std_Logic_Vector(n_max downto 1);
        -- sync_rot to become 1 or 2 bit registry (depending on n_max)
        state_node_sync : in Std_Logic_Vector(log2_n_max downto 1);
        -- these are input registers 
        -- register in continuous mode only?
--        period_ber : in Std_Logic_Vector(24 downto 1);  
        -- if I decide to make the decision in / out of node synch in software
        -- then I can't spare these registers. I'll do this for SOPC
--        threshold_ns, period_ns : in Std_Logic_Vector(8 downto 1);
        -- to remain : input control register
        sel_code : in Std_Logic_Vector(sel_code_size downto 1);
        -- tr_init and tb_type from block decoder. To become control register in SOPC
        tr_init_state : in Std_Logic_Vector(constraint_length_m_1 downto 1);
        -- tb_type : in Std_Logic;
        -- what about these two? Do someone REALLY needs this? I doubt it.
        -- specially the bm_init_value. Definitely an option is required to 
        -- generate the logic for bm_init_state.
        bm_init_state : in Std_Logic_Vector(constraint_length_m_1 downto 1);
        bm_init_value : in Std_Logic_Vector(bmgwide downto 1);
        -- new register control input ports to set traceback length and bitsout and 
        -- even the contraint_length
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);

        -- old interface to be replaced by atlantic
        -- what about normalize? it is a flag that indicates 
        -- state metrics have been substracted 2 ^ (bmg -1)
        -- need a counter of normalize occurrences as output register
        -- this value multiplied by 2^(bmg-1) + final state metric value
        -- would give an indication of how noisy is the incoming data.
        normalizations : out Std_Logic_Vector(8 downto 1);

        -- Slave source side control signals
        source_rdy : in Std_Logic; 
        source_val : out Std_Logic; 
        source_sop : out Std_Logic; 
        source_eop : out Std_Logic; 

        -- a single bit for atlantic for output data
        decbit : out Std_Logic;
        -- in / out sync Probably to be removed for sopc, leave them for atlantic
--        in_sync, out_sync : out Std_Logic;
        -- These two from block decoder and status register for SOPC
        -- maybe options to disconnect them ?
        bestmet : out Std_Logic_Vector(bmgwide downto 1);
        bestadd : out Std_Logic_Vector(constraint_length_m_1 downto 1);
        -- register output: number of errors, for atlantic leave current bererr
        -- but for sopc bererr can be calculated by the processor for every channel
        -- by the way what happens with bererr in atlantic if multicode is used?
        -- and if they are blocks?
        -- The block decoder doesn't have bererr
--        numerr_ns : out Std_Logic_Vector(8 downto 1);
         numerr : out Std_Logic_Vector(numerr_size downto 1)
    );    
end entity auk_vit_hyb_top_atl;    


Architecture rtl of auk_vit_hyb_top_atl is


Constant L_max : NATURAL := constraint_length_m_1+1;
Constant GND : Std_Logic := '0';
Constant maxstates : NATURAL := 2**(L_max-1);
Constant log2_cc : NATURAL := L_max-2-LOG2_ceil_table(ACS_units);
Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant n_min : NATURAL := Get_n_min(n_list);
Constant family    : STRING := "Stratix";
--Constant sm_init_logic : STRING := "used";


-- Component instantiated by Turbo autoplace on 24/01/2003 at 19:06:42
COMPONENT auk_vit_hyb_trb_atl
    Generic (
        L : NATURAL := 5;
        ACS_units : NATURAL := 1;
        v : NATURAL := 50;
        vlog_wide : NATURAL := 6;
        use_altera_syncram  : NATURAL := 1
    );
    Port (
        clk, reset, survready : in Std_Logic;
        latch_best_mark : in Std_Logic;
        ena_slave_source, sink_sop_q : in Std_Logic; -- output in Master mode, input in Slave mode
        allow_ena_assert : out Std_Logic; -- input in Master mode, output in Slave mode
        val_source, sop_source, eop_source : out Std_Logic; -- regardless of Master or Slave mode
    dav_slave_source : out Std_Logic;
        survive : in Std_Logic_Vector(2**(L-1) downto 1);
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);
        tr_init_state : in Std_Logic_Vector(L-1 downto 1);
        sink_eop_q : in Std_Logic;
        bestadd : in Std_Logic_Vector(L-1 downto 1);
        block_delimeters : in Std_Logic;
         decbit : out Std_Logic
    );    
END COMPONENT;



-- Component instantiated by Turbo autoplace on 14/07/2003 at 16:37:38
COMPONENT auk_vit_hyb_ber_atl
    Generic (
                n : STRING := "2";
                L                     : STRING  := "5";
                dec_modes        : STRING  := "V"; --"V_V_V_T";
                ncodes : NATURAL := 1;
                n_max : NATURAL := 2;
                L_max : NATURAL := 5;
                vlog_wide : NATURAL := 6;
                size_bus    : NATURAL := 16; -- 16 or 32
                sel_code_size : NATURAL := 1;
                numerr_size  : NATURAL := 12; -- depens on how big the block is going to be
                 v : NATURAL := 25;
                ga : STRING := "19";
                gb : STRING := "29";
                gc : STRING := "0";
                gd : STRING := "0";
                ge : STRING := "0";
                gf : STRING := "0";
                gg : STRING := "0";
                dev_family : STRING := "Stratix"
    );
    Port (
        clk   : in std_logic;
        reset : in Std_Logic;
        ber_clear : in std_logic;
        sink_val, outvalid, sop_source : in Std_Logic;
        data_ram_in : in Std_Logic_Vector(2*n_max downto 1);
        sel_code : in Std_Logic_Vector(sel_code_size downto 1);
        decbit : in Std_Logic;
        allow_ena_assert : out std_logic;
        numerr : out Std_Logic_Vector(numerr_size downto 1)
    );    
END COMPONENT;



-- Component instantiated by Turbo autoplace on 20/03/2003 at 12:14:08
COMPONENT auk_vit_hyb_bmp_atl
    Generic (
                n                     : STRING  := "2_2_3_4";
                L                     : STRING  := "3_5_5_5";
                modes                : STRING  := "V_V_V_T";
                ncodes : NATURAL := 2;
                n_max : NATURAL := 2;
                log2_n_max : natural := 1;
                L_max : NATURAL := 7;
                ACS_units : NATURAL := 2;
                 softbits : NATURAL := 4;
                bmgwide : NATURAL := 12;
                BER : STRING := "used";
                node_sync : STRING := "unused";
                sm_init_logic : STRING := "used";
                ga                     : STRING := "5_19_21_21";
                gb                     : STRING := "7_29_27_23";
                gc                     : STRING := "0_0_31_27";
                gd                     : STRING := "0_0_0_31";
                ge                     : STRING := "0_0_0_0";
                gf                     : STRING := "0_0_0_0";
                gg                     : STRING := "0_0_0_0";
                dev_family : STRING := "Stratix"
    );
    Port (
        clk, reset : in Std_Logic;

        dav_master_sink : in Std_Logic; -- input in Master mode, output in Slave mode
        ena_master_sink : out Std_Logic; -- output in Master mode, input in Slave mode
        val_sink : in Std_Logic; -- only used in Master Sink
        sop_sink : in Std_Logic; -- regardless of Master or Slave mode
        eop_sink : in Std_Logic; -- regardless of Master or Slave mode
        allow_ena_assert_from_trb : in Std_Logic;

        -- what about these two? Do someone REALLY needs this? Maybe.
        -- specially the bm_init_state. Definitely an option is required to 
        -- generate the logic for bm_init_state.
        bm_init_state : in Std_Logic_Vector(L_max-1 downto 1);
        bm_init_value : in Std_Logic_Vector(bmgwide downto 1);
        tr_init_state : in Std_Logic_Vector(L_max-1 downto 1);
        state_node_sync : in Std_Logic_Vector(log2_n_max downto 1);

         sel_code : in Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
        data_for_BER : out Std_Logic_Vector(2*n_max downto 1);
        rr : in Std_Logic_Vector(n_max*softbits downto 1);
        eras_sym : in Std_Logic_Vector(n_max downto 1);
        survready, latch_best_mark : out Std_Logic;
        normalizations : out Std_Logic_Vector(8 downto 1);
         survive : out Std_Logic_Vector(2**(L_max-1) downto 1);
        bestmet : out Std_Logic_Vector(bmgwide downto 1);
        bestadd : out Std_Logic_Vector(L_max-1 downto 1);
        block_delimeters : out Std_Logic
    );    
END COMPONENT;



signal tb_length_q       : Std_Logic_Vector(vlog_wide downto 1);
Signal state_node_sync_q : Std_Logic_Vector(log2_n_max downto 1);
Signal sel_code_q        : Std_Logic_Vector(sel_code_size downto 1);
Signal tr_init_state_q   : Std_Logic_Vector(constraint_length_m_1 downto 1);
--Signal tb_type_q : Std_Logic;
Signal bm_init_state_q   : Std_Logic_Vector(L_max-1 downto 1);
Signal bm_init_value_q   : Std_Logic_Vector(bmgwide downto 1);
Signal data_for_BER      : Std_Logic_Vector(2*n_max downto 1);
Signal survive           : Std_Logic_Vector(maxstates downto 1);
Signal survready         : Std_Logic;
Signal decbitnode        : Std_Logic;
Signal bestadd_int       : Std_Logic_Vector(L_max-1 downto 1);
Signal allow_ena_assert, latch_best_mark : Std_Logic;
Signal allow_ena_assert_from_ber, allow_ena_assert_from_trb     : Std_logic;
Signal sink_sop_q, sink_eop_q, source_sop_node, source_val_node : Std_Logic;
Signal block_delimeters  : Std_Logic;
Signal bestmet_int       : Std_Logic_Vector(bmgwide downto 1);
Signal sink_dav_master   : Std_Logic;



begin

sink_dav_master <= '1';

ASSERT ((n_min>1) and (n_min<8) and (n_max>1) and (n_max<8))
-- coverage off 
    REPORT         "Parameter n must be between 2 and 7" 
    SEVERITY        ERROR;
-- coverage on
ASSERT ((BER="used") or (BER="unused"))
-- coverage off 
    REPORT         "Parameter BER has to be either used or unused" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (ACS_units=1) or (ACS_units=2) or (ACS_units=4) or (ACS_units=8) or (ACS_units=16)
-- coverage off
    REPORT         "Parameter ACS_units can take only values 1, 2, 4, 8 or 16" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (log2_cc>2)
-- coverage off
    REPORT         "Expresion L-2-LOG2_ceil_table(ACS_units) must be at least 3. "& 
                         "Either reduce ACS_units or increase L" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (L_max<10)
-- coverage off
    REPORT         "Parameter L_max cannot be bigger than 10" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (L_max>4)
-- coverage off
    REPORT         "Parameter L_max cannot be lower than 5" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (v>7) 
-- coverage off
    REPORT         "Parameter v must be at least 8" 
    SEVERITY        ERROR;
-- coverage on

clk_atl: Process (clk, reset)
    begin
    if reset='1' then
        -- (master) sink side
        sink_sop_q <= '0';
        sink_eop_q <= '0';
        tb_length_q <= (others => '0');
        state_node_sync_q <= (others => '0');
        sel_code_q <= (others => '0');
        tr_init_state_q <= (others => '0');
        --tb_type_q <= '0';
        bm_init_state_q <= (others => '0');
        bm_init_value_q <= (others => '0');

    elsif Rising_edge(clk) then
        if sink_val='1' then
          sink_sop_q <= sink_sop;
          sink_eop_q <= sink_eop;
        end if;
        if sink_sop='1' and sink_val='1' then
            tb_length_q <= tb_length;
            state_node_sync_q <= state_node_sync;
            sel_code_q <= sel_code;
            bm_init_state_q <= bm_init_state;
            bm_init_value_q <= bm_init_value;
        end if;
        -- tr_init_state latched at the end of the block
        -- remember to document
        if sink_eop='1' and sink_val='1' then
            tr_init_state_q <= tr_init_state;
            --tb_type_q <= tb_type;
        end if;
    end if;

end process clk_atl;




metric_processing : auk_vit_hyb_bmp_atl
    Generic map (n => n, L => L, L_max => L_max, softbits => softbits,
                             log2_n_max => log2_n_max, modes => dec_mode,
                             ACS_units => ACS_units, bmgwide => bmgwide, dev_family => family,
                             BER => BER, node_sync => node_sync, 
                             ncodes => ncodes, n_max => n_max, sm_init_logic => sm_init_logic,
                             ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)
    Port map (clk => clk, reset => reset, 
                        dav_master_sink => sink_dav_master, 
                        ena_master_sink => sink_rdy, 
                        val_sink => sink_val,    
                        sop_sink => sink_sop_q,
                        eop_sink => sink_eop,
                        allow_ena_assert_from_trb => allow_ena_assert,
                        bm_init_state    => bm_init_state_q, 
                        bm_init_value => bm_init_value_q,
                        tr_init_state => tr_init_state_q,
                        latch_best_mark => latch_best_mark,
                        state_node_sync => state_node_sync_q,
                         rr => rr, sel_code => sel_code_q, 
                        bestadd => bestadd_int, 
                        bestmet => bestmet_int,
                        eras_sym => eras_sym,
                        survready => survready, --we_ram_sur_out => we_ram_sur,
                        normalizations => normalizations,
                        block_delimeters => block_delimeters,
                         survive => survive, data_for_BER => data_for_BER);

traceback : auk_vit_hyb_trb_atl
    Generic map (L => L_max, v => v, 
                 vlog_wide => vlog_wide, 
                             ACS_units => ACS_units,  use_altera_syncram => use_altera_syncram)
    Port map (clk => clk, reset => reset, survready => survready,
                        latch_best_mark => latch_best_mark,
                        ena_slave_source => source_rdy,
                        sink_sop_q => sink_sop_q,
                        allow_ena_assert => allow_ena_assert_from_trb,
                        val_source => source_val_node,
                        sop_source => source_sop_node,
                        eop_source => source_eop,
                        dav_slave_source => open, --source_dav_slave,
                        survive => survive, 
                        tb_length => tb_length_q, 
                        tr_init_state => tr_init_state_q, sink_eop_q => sink_eop_q,
                        block_delimeters => block_delimeters,
                         decbit => decbitnode,
                         bestadd => bestadd_int    );    


                        
allow_ena_assert <= allow_ena_assert_from_ber and allow_ena_assert_from_trb;

ber_not_used: if BER="unused" or BER="UNUSED" generate
    allow_ena_assert_from_ber <= '1';
end generate ber_not_used;

ber_used: if BER="used" or BER="USED" generate

ber_measurement : auk_vit_hyb_ber_atl
    Generic map (n => n, L => L, dec_modes => dec_mode, ncodes => ncodes, 
                             dev_family => family, sel_code_size => sel_code_size,
                             n_max => n_max, L_max => L_max, v => v, numerr_size => numerr_size, 
                             vlog_wide => vlog_wide,
                             ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)
    Port map (
        clk => clk, 
        reset => reset, 
        ber_clear => ber_clear,
        -- I think I need a pre of source_val
        outvalid => source_val_node, 
        sop_source => source_sop_node,
        sink_val => sink_val,
        sel_code => sel_code, 
        data_ram_in => data_for_BER, decbit => decbitnode,
        allow_ena_assert => allow_ena_assert_from_ber,
        numerr => numerr);

end generate ber_used;

ber_notused: if BER="unused" or BER="UNUSED" generate
    numerr <= (others => '0');
end generate ber_notused;



latch_reg : Process(clk, reset)
begin
if reset='1' then
  bestadd <= (others => '0');
    --bestmet <= (others => '0');
elsif Rising_edge(clk) then
  if latch_best_mark='1' then
    bestadd <= bestadd_int;
        --bestmet <= bestmet_int;
  end if;
end if;
end process latch_reg;          

bestmet <= bestmet_int;


decbit <= decbitnode;
source_sop <= source_sop_node;
source_val <= source_val_node;

end architecture rtl;    
