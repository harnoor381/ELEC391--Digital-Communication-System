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
-- $RCSfile: auk_vit_par_trb_atl_arc_mem.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/Parallel/atlantic/auk_vit_par_trb_atl_arc_mem.vhd,v $
--
-- $Revision: #2 $
-- $Date: 2008/11/25 $
-- Check in by           : $Author: dtdnguye $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description    :  Trace Back entity for Parallel Viterbi Decoder
--
-- ALTERA Confidential and Proprietary
-- Copyright 2003 (c) Altera Corporation
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

Entity auk_vit_par_trb_atl is
    Generic (
        L : NATURAL := 9;
        v : NATURAL := 50;
        ncodes : NATURAL := 1;
        vlog_wide : NATURAL := 6;
        ini_filename        : STRING  := "filename.hex";
        use_altera_syncram  : NATURAL := 1
    );
    Port (
        clk    : in std_logic; 
        reset  : in std_logic;
        enable : in std_logic;
        survready, baddready : in Std_Logic;
        --ena_slave_source : in std_logic;
        sink_sop_q : in Std_Logic; 
        sink_eop_del : in Std_Logic;
        allow_ena_assert : out Std_Logic;
        data_available   : out std_logic;
        --val_source : out std_logic; 
        sop_source, eop_source : out Std_Logic; 
        survive : in Std_Logic_Vector(2**(L-1) downto 1);
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);
        tr_init_state : in Std_Logic_Vector(L-1 downto 1);
        tb_type : in Std_Logic;
        bestadd : in Std_Logic_Vector(L-1 downto 1);
         decbit : out Std_Logic
    );    
end entity auk_vit_par_trb_atl;    

Architecture rtl_mem of auk_vit_par_trb_atl is

Constant VCC : Std_Logic := '1';
Constant maxstates : NATURAL := 2**(L-1);
Constant halfstates : NATURAL := 2**(L-2);
-- I may have to increse v -> (v+17) to avoid lapses of saturation
-- also 17 because of vlog_wide coming from the MW!!
Constant log2_4v : NATURAL := LOG2_ceil_table(4*(v+17));
-- Alex 18-01-2007  After the information that Neil gave me, I have to check NumWords usage 
-- to minimize whenever possible the amount of M4K being used  Here and in all other memory instantiations
Constant NumWords : natural := 2**log2_4v;
Constant Numbits  : natural := 2**log2_4v;
--Constant fifo_pointers_wide : NATURAL := 3;

--Constant vector_one : Std_Logic_Vector(vlog_wide downto 1) := --natural_2_m(arg => 1, size => vlog_wide);
--    CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => vlog_wide), SIZE => vlog_wide);
Constant one_maxstates : Std_Logic_Vector(maxstates downto 1) := --natural_2_m(arg => 1, size => maxstates);
    CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => maxstates), SIZE => maxstates);
Constant count_limit : std_logic_vector(log2_4v downto 1) := --natural_2_m(arg => 2**log2_4v - 8, size => log2_4v);
    CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**log2_4v - 8, SIZE => log2_4v), SIZE => log2_4v);

Subtype vector_4v is Std_Logic_Vector(log2_4v downto 1);
Subtype vector_2  is Std_Logic_Vector(2       downto 1);
type matrix_4v is array(NATURAL RANGE <>) of vector_4v;
type matrix_2  is array(NATURAL RANGE <>) of vector_2;

COMPONENT auk_vit_sel
    generic ( inwidth, outwidth: NATURAL );
    port (
        selin : in Std_Logic_Vector(inwidth downto 1);
        selout : out Std_Logic_Vector(outwidth downto 1)
        );
END COMPONENT;


signal readtop : Std_Logic_Vector(log2_4v downto 1);
signal readbot : Std_Logic_Vector(log2_4v downto 1);
signal tracetop_q, tracetop_d, tracebot_q, tracebot_d : Std_Logic_Vector(maxstates downto 1);
signal stop, sbot : Std_Logic_Vector(maxstates downto 1);
Signal decbittop_int, decbitbot_int : Std_Logic_Vector(1 downto 1);
Signal load_top_trb_eng, load_bot_trb_eng : Std_Logic;
Signal decbittop_swap, decbitbot_swap : Std_Logic_Vector(1 downto 1);
signal valid_top_bits_seed, valid_bot_bits_seed : Std_Logic;
signal valid_top_bits, valid_bot_bits : Std_Logic_Vector(4 downto 1);
signal write_top_bits, read_top_bits, stop_bits_top : Std_Logic_Vector(log2_4v downto 1);
signal write_bot_bits, read_bot_bits, stop_bits_bot : Std_Logic_Vector(log2_4v downto 1);

Signal tb_length_latch_early : Std_Logic_Vector(vlog_wide downto 1);
Signal tb_length_latch_later_top, tb_length_latch_later_bot : std_logic_vector(vlog_wide downto 1);
signal tb_length_plus_early : Std_Logic_Vector(vlog_wide+1 downto 1);
signal tb_length_plus_later_top, tb_length_plus_later_bot : std_logic_vector(vlog_wide+1 downto 1);
signal two_by_tb_length : Std_Logic_Vector(vlog_wide+2 downto 1);
signal readadd, writeadd, writeadd_del, startadd : std_logic_vector(log2_4v downto 1); 
Signal stopadd_top, stopadd_bot, startadd_prev, symbol_count : Std_Logic_Vector(log2_4v downto 1);
signal statewide : Std_Logic_Vector(maxstates downto 1);
signal decbit_q : Std_Logic;
signal init_startadd : Std_Logic;
signal init_readtop, init_readbot : Std_Logic;
Signal rd_eq_stop_top, rd_eq_stop_bot : Std_Logic;
Signal bestadd_int, bestadd_out : Std_Logic_Vector(L-1 downto 1);
signal decbit_shunt, ena_trb_pu : Std_Logic;

signal address_pointer_out, next_stopadd : Std_Logic_Vector(log2_4v downto 1);
signal inc_startadd_d, init_next_stopadd : std_logic; 
signal wr_fifo, val_source_q : Std_Logic;
signal dav_bot_eng_del, dav_top_eng_del : Std_Logic_Vector(2 downto 1);
--signal sop_source_shunt, eop_source_shunt : Std_Logic;
signal allow_val_assert, allow_next_sop : Std_Logic;
signal load_readtop, load_readbot : Std_Logic;
signal allow_ena_assert_int : Std_Logic;
Signal inc_stopadd_top,  inc_stopadd_bot : Std_Logic;
Signal init_stopadd_top, init_stopadd_bot : Std_Logic;
signal wr_eg_start, wr_eg_start_q : Std_Logic;
signal chunk_top_ready, chunk_bot_ready : std_logic; 
signal rd_eq_stop_top_bits, rd_eq_stop_bot_bits : Std_Logic;
Signal dav_top_eng, dav_bot_eng, sink_eop_del_del : Std_Logic;
signal eop_bot_indicator, eop_top_indicator : Std_Logic;
signal ena_read_top_bits, ena_read_bot_bits : Std_Logic;
signal load_read_top_bits, load_read_bot_bits : Std_Logic;
signal sop_source_gen, eop_source_gen : Std_Logic;
signal eop_source_pipe, sop_source_pipe : Std_Logic_Vector(2 downto 1);
Signal data_val_pipe : Std_Logic_Vector(3 downto 1);
signal ena_readtop, ena_readbot : std_logic; 
Signal dat_source_int_d, dav_source_int : Std_Logic;
signal data_val_shunt, source_ena, source_val_int : Std_Logic;

Type atl_buffer_fsm is (S0, out_idle, out_active, out_hold);

type read_counter_states is (idle, load, loaded, working); 

Type fsm_trb_states is (S0, init, fill,  wait_new, eop_handed, new_block, hand_over);

Type eng_seq_states is (idle, init_rd, init_rd_eop,  
                                                init_trb_engine, wait0, go_trb_engine);

-- signal atl_buffer_state, atl_buffer_next_state : atl_buffer_fsm;
Signal state_read_top_bits, next_state_read_top_bits : read_counter_states;
Signal state_read_bot_bits, next_state_read_bot_bits : read_counter_states;
Signal state_trb, next_state_trb : fsm_trb_states;
Signal state_top_eng_seq, next_state_top_eng_seq : eng_seq_states;
Signal state_bot_eng_seq, next_state_bot_eng_seq : eng_seq_states;

signal out_fsm_buf : Std_Logic_Vector(3 downto 1);
signal out_fsm_trb, out_fsm_bot_eng_seq, out_fsm_top_eng_seq : Std_Logic_Vector(4 downto 1);
-- logic fifos starting name with lf_
signal lf_addr_ptr : matrix_4v(1 to 2);
Signal lf_pull_addr_ptr, lf_push_addr_ptr : std_logic;
signal lf_status_addr_ptr : std_logic_vector(3 downto 1);
signal lf_job_start : matrix_2(1 to 6);
Signal lf_pull_job_start, lf_push_job_start : std_logic;
signal lf_status_job_start : std_logic_vector(7 downto 1);
signal lf_addr_top_bits_ptr : matrix_4v(1 to 4);
Signal lf_pull_addr_top_bits_ptr, lf_push_addr_top_bits_ptr : std_logic;
signal lf_status_addr_top_bits_ptr : std_logic_vector(5 downto 1);
signal lf_addr_bot_bits_ptr : matrix_4v(1 to 4);
Signal lf_pull_addr_bot_bits_ptr, lf_push_addr_bot_bits_ptr : std_logic;
signal lf_status_addr_bot_bits_ptr : std_logic_vector(5 downto 1);
signal lf_pull_sop_ptr, lf_push_sop_ptr : std_logic;
signal lf_status_sop_ptr : std_logic_vector(4 downto 1);
signal lf_pull_eop_ptr, lf_push_eop_ptr : std_logic;
signal lf_status_eop_ptr : std_logic_vector(4 downto 1);
Signal which_eng_use, sink_sop_q_del : std_logic;

begin

--VCC_signal <= VCC;

ena_trb_pu <= enable;

clk_registers : Process (clk, reset)

begin
if reset = '1' then
    sink_eop_del_del <= '0'; 
    tb_length_latch_early <= (others => '0');
    tb_length_latch_later_top <= (others => '0');
    tb_length_latch_later_bot <= (others => '0');
    sink_sop_q_del <= '0';
elsif Rising_edge(clk) then
    sink_eop_del_del <= sink_eop_del;
    sink_sop_q_del <= sink_sop_q;
    -- this one with latch from newblock
    --if state_trb=S0 or state_trb=eop_handed or sink_sop_q='1' then
    if (state_trb=wait_new and lf_status_sop_ptr(1)='0') or state_trb=S0 then
        tb_length_latch_early <= tb_length;
    end if;
    -- this one will latch from early at the begining of a trb job
    --if state_trb=S0 or state_trb=new_block then
    if load_readtop='1' then
        tb_length_latch_later_top <= tb_length_latch_early;
    end if;
    if load_readbot='1' then
        tb_length_latch_later_bot <= tb_length_latch_early;
    end if;
end if;
end process clk_registers;

-- registered at top level
tb_length_plus_early <= unsigned('0' & tb_length_latch_early) + natural(4);
tb_length_plus_later_top <= unsigned('0' & tb_length_latch_later_top) + natural(4);
tb_length_plus_later_bot <= unsigned('0' & tb_length_latch_later_bot) + natural(4);


which_eng_use_gen: process(state_top_eng_seq)
begin
if state_top_eng_seq=init_rd_eop or state_top_eng_seq=init_rd then --or state_top_eng_seq=wait0 then
    which_eng_use <= '1';
else
    which_eng_use <= '0';
end if;
end process which_eng_use_gen;

counters : Process(clk, reset)

    variable eop_bot_indicator_var, eop_top_indicator_var : Std_Logic;

begin
if reset='1' then
    readtop <= (others => '0');
    readbot <= (others => '0');
    writeadd <= (others => '0');
    writeadd_del <= (others => '0');
    symbol_count <= (others => '0');
    write_top_bits <= (others => '0');
    write_bot_bits <= (others => '0');
    read_top_bits <= (others => '0');
    read_bot_bits <= (others => '0');
    eop_bot_indicator <= '0';
    eop_top_indicator <= '0';
elsif Rising_edge(clk) then
    if init_readtop='1' then
        readtop <= address_pointer_out;
    elsif load_readtop='1' then
    -- this turns out to be a problem the tb_length changes
    -- I have to look for an alternative.
        readtop <= unsigned(next_stopadd) + unsigned(two_by_tb_length); --startadd;
    elsif ena_readtop='1' then
        readtop <= readtop - natural(1);  -- DOWN
    end if;
    if init_readbot='1' then
        readbot <= address_pointer_out;
    elsif load_readbot='1' then
        readbot <= unsigned(next_stopadd)+unsigned(two_by_tb_length); -- startadd;
    elsif ena_readbot='1' then
        readbot <= unsigned(readbot) - natural(1);  -- DOWN
    end if;
    if survready='1' then
       writeadd <= unsigned(writeadd) + natural(1);
  end if;
    if baddready='1' then
       writeadd_del <= unsigned(writeadd_del) + natural(1);
  end if;
    -- 23-01-2007  Alex
    -- source_val_int  here makes "difficult" to move out the atlantic source into a separate
    --  functional unit, I should consider using the internal signal "data_available" 
    -- AND enable from the controller for this.
    --if survready='1' and source_val_int='0' then
    if survready='1' and dav_source_int='0' and ena_trb_pu='1' then
       symbol_count <= unsigned(symbol_count) + natural(1);
    --elsif survready='0' and source_val_int='1' then
    -- Duong: SPR 283847, I added one more condition "unsigned(symbol_count)>0" to
    -- prevent the counter from counting down to -1.
    elsif survready='0' and dav_source_int='1' and ena_trb_pu='1' and unsigned(symbol_count)>0 then
    
       symbol_count <= unsigned(symbol_count) - natural(1);
  end if;
    if valid_top_bits(2)='1' then
        write_top_bits <= unsigned(write_top_bits) + natural(1);
    end if;
    if valid_bot_bits(2)='1' then
        write_bot_bits <= unsigned(write_bot_bits) + natural(1);
    end if;
    
    if load_read_top_bits='1' then
        read_top_bits <= lf_addr_top_bits_ptr(1); 
    elsif ena_read_top_bits='1' and ena_trb_pu='1' then --and ena_read_bot_extend='0' then
        read_top_bits <= unsigned(read_top_bits) - natural(1);
    end if;
    if load_read_bot_bits='1' then
        read_bot_bits <= lf_addr_bot_bits_ptr(1); 
    elsif ena_read_bot_bits='1' and ena_trb_pu='1' then --and ena_read_top_extend='0' then
        read_bot_bits <= unsigned(read_bot_bits) - natural(1);
    end if;
    --if ena_trb_pu='1' then
    eop_bot_indicator_var := eop_bot_indicator;
    eop_top_indicator_var := eop_top_indicator;
    if state_top_eng_seq=init_rd_eop then
        eop_top_indicator_var := '1';
    elsif rd_eq_stop_top='1' then
        eop_top_indicator_var := '0';
    end if;
    eop_top_indicator <= eop_top_indicator_var;
    if state_bot_eng_seq=init_rd_eop then
        eop_bot_indicator_var := '1';
    elsif rd_eq_stop_bot='1' then
        eop_bot_indicator_var := '0';
    end if;
    eop_bot_indicator <= eop_bot_indicator_var;

end if;
end process counters;          


two_by_tb_length <= unsigned('0' & tb_length_plus_early) + unsigned('0' & tb_length_latch_early);
--two_by_tb_length(vlog_wide+2 downto 2) <= tb_length_plus_early;
--two_by_tb_length(1) <= '0';


init_stopadd_top <= load_readtop or init_readtop;
init_stopadd_bot <= load_readbot or init_readbot;

start_stop_proc : Process (clk, reset)
begin
if reset = '1' then
    startadd <= (others => '0');
    startadd_prev <= (others => '0');
    stopadd_top  <= (others => '0');
    stopadd_bot  <= (others => '0');
    next_stopadd <= (others => '0');
    valid_top_bits <= (others => '0');
    valid_bot_bits <= (others => '0');
elsif Rising_edge(clk) then
        valid_top_bits(1) <= valid_top_bits_seed;
        valid_bot_bits(1) <= valid_bot_bits_seed;
        valid_top_bits(2) <= valid_top_bits(1);
        valid_bot_bits(2) <= valid_bot_bits(1);
        valid_top_bits(3) <= valid_top_bits(2);
        valid_bot_bits(3) <= valid_bot_bits(2);
        valid_top_bits(4) <= valid_top_bits(3);
        valid_bot_bits(4) <= valid_bot_bits(3);
        
-- I need to remove this dependency on address_pointer_out
--  at init time because it depens on a init file on RAM which 
--  I want to remove for the sake of this core being solid and trouble free
        
        if init_startadd='1' then
        startadd <= unsigned(address_pointer_out) + unsigned(two_by_tb_length);
        elsif inc_startadd_d='1' then
            startadd <= unsigned(startadd) + unsigned(tb_length_plus_early);
        end if;
        if inc_startadd_d='1' then
            startadd_prev <= startadd;
        elsif init_startadd='1' then
            startadd_prev <= address_pointer_out;
        end if;

        if init_stopadd_top='1' then
            stopadd_top <= next_stopadd; 
        end if;

    -- For both stopadd_top and _bot there is a third initialization case:
    -- when the other has been initilized at the begining of new block with
    -- address_pointer+1; the next one being either top or bot has to 
    -- initialize with _top | _bot + tb_length
        if init_stopadd_bot='1' then
            stopadd_bot <= next_stopadd; 
        end if;
        if init_next_stopadd='1' then
            next_stopadd <= unsigned(address_pointer_out) + natural(1);
        elsif inc_startadd_d='1' then
            next_stopadd <= unsigned(next_stopadd) + unsigned(tb_length_plus_early);
        end if;
        
end if;
end process start_stop_proc;


mux_readadd: process(address_pointer_out, lf_status_addr_ptr(1), startadd_prev)   

begin
    if lf_status_addr_ptr(1)='0' then
        readadd <= address_pointer_out;
    else
        readadd <= startadd_prev;
    end if;
end process mux_readadd;


eq_comp5_nolatch : Process (writeadd_del, startadd_prev, startadd)

    variable diff_1, diff_2 : Std_Logic_Vector(log2_4v downto 1);

begin
    diff_1 := unsigned(writeadd_del) - unsigned(startadd_prev);
    diff_2 := unsigned(startadd) - unsigned(startadd_prev);

    if (unsigned(diff_1) > unsigned(diff_2)) then 
        wr_eg_start_q <= '1';
    else
        wr_eg_start_q <= '0';
    end if;

end process eq_comp5_nolatch; 

wr_eg_start <= wr_eg_start_q; 


eq_comp6: process(readtop, stopadd_top, readbot, stopadd_bot, tb_length_plus_later_bot, 
                                    tb_length_plus_later_top, eop_top_indicator, eop_bot_indicator,
                                    state_bot_eng_seq, state_top_eng_seq )

begin
    if unsigned(readtop) = unsigned(stopadd_top) then
        rd_eq_stop_top <= '1';
    else
        rd_eq_stop_top <= '0';
    end if;
    if unsigned(readbot) = unsigned(stopadd_bot) then
        rd_eq_stop_bot <= '1';
    else
        rd_eq_stop_bot <= '0';
    end if;

-- eop_top_indicator -> 0 if non-eop, 1 if eop block
    if eop_top_indicator='1' and 
        (state_top_eng_seq=init_trb_engine or state_top_eng_seq=wait0 or state_top_eng_seq=go_trb_engine) then
        valid_top_bits_seed <= '1';
    elsif eop_top_indicator='0' and state_top_eng_seq/=idle and state_top_eng_seq/=init_rd and
                state_top_eng_seq/=init_rd_eop then
    if (unsigned(readtop) - unsigned(stopadd_top)) < unsigned(tb_length_plus_later_top) then
        valid_top_bits_seed <= '1';
    else
            valid_top_bits_seed <= '0';
    end if;
  else
        valid_top_bits_seed <= '0';
    end if;

    if eop_bot_indicator='1' and 
        (state_bot_eng_seq=init_trb_engine or state_bot_eng_seq=wait0 or state_bot_eng_seq=go_trb_engine) then
-- the last chunk in a block doesn't depend on tb_length one it is started.
-- Hence the state_trb can latch a new tb_length at eop_handed state
        valid_bot_bits_seed <= '1';

    elsif eop_bot_indicator='0' and state_bot_eng_seq/=idle and state_bot_eng_seq/=init_rd and
                state_bot_eng_seq/=init_rd_eop then
    if (unsigned(readbot) - unsigned(stopadd_bot)) < unsigned(tb_length_plus_later_bot) then
        valid_bot_bits_seed <= '1';
    else
            valid_bot_bits_seed <= '0';
    end if;
    else
        valid_bot_bits_seed <= '0';
  end if;
end process eq_comp6;

eq_comp10: process(read_top_bits, stop_bits_top, read_bot_bits, stop_bits_bot)

begin
    if unsigned(read_top_bits) = unsigned(stop_bits_top) then
        rd_eq_stop_top_bits <= '1';
    else
        rd_eq_stop_top_bits <= '0';
    end if;
    if unsigned(read_bot_bits) = unsigned(stop_bits_bot) then
        rd_eq_stop_bot_bits <= '1';
    else
        rd_eq_stop_bot_bits <= '0';
    end if;
end process eq_comp10;


chunk_top_ready <= valid_top_bits(2) and not valid_top_bits(1);
chunk_bot_ready <= valid_bot_bits(2) and not valid_bot_bits(1);



FSM_trb: process(state_trb, wr_eg_start, lf_status_addr_ptr(1),
                                 eop_top_indicator, eop_bot_indicator,
                                 state_top_eng_seq, state_bot_eng_seq, 
                                 lf_status_sop_ptr(1), survready,
                                 sink_sop_q) 

    begin
        case state_trb is
        when S0 =>
            if sink_sop_q='1' then
                next_state_trb <= init;
            else
                next_state_trb <= S0;
            end if;
        when init =>
            next_state_trb <= fill;
        when fill =>
            if lf_status_addr_ptr(1)='0' and 
                (state_top_eng_seq=init_rd_eop or state_bot_eng_seq=init_rd_eop) then
                    next_state_trb <= eop_handed;
            elsif wr_eg_start='1' and survready='1' and lf_status_addr_ptr(1)='1' and 
            (state_top_eng_seq=init_rd or state_top_eng_seq=idle or state_bot_eng_seq=init_rd or state_bot_eng_seq=idle) then
                next_state_trb <= hand_over;
            else
                next_state_trb <= fill;
            end if;
        when eop_handed => 
            next_state_trb <= wait_new;
        when wait_new => 
        -- I think this wait ought to be for a new sink_sop_q or checking that 
        -- the status_Sop has a backlog of pulses in there so operation can carry on
        -- init_startadd will be then moved to new_block from eop_handed.
            if lf_status_sop_ptr(1)='0' then
              next_state_trb <= new_block;
            else
                next_state_trb <= wait_new;
            end if;
        when new_block => 
            next_state_trb <= fill;
        when hand_over =>
            next_state_trb <= fill; 

        -- coverage off
        when others => next_state_trb <= S0;
        -- coverage on
        end case;
        
    end process FSM_trb;


FSM_top_eng_seq: process(state_top_eng_seq, state_bot_eng_seq, state_trb, survready,
                                                 wr_eg_start, rd_eq_stop_top, rd_eq_stop_bot, lf_status_eop_ptr(1), lf_status_addr_top_bits_ptr(5 downto 4)) 


begin
    case state_top_eng_seq is

    when idle =>
        if lf_status_eop_ptr(1)='0' and state_trb=fill and state_bot_eng_seq/=init_rd_eop and lf_status_addr_top_bits_ptr(5 downto 4)="00" then
            next_state_top_eng_seq <= init_rd_eop;
        elsif wr_eg_start='1' and    lf_status_eop_ptr(1)='1' and state_trb=fill and survready='1' then
        next_state_top_eng_seq <= init_rd;
        else
            next_state_top_eng_seq <= idle;
        end if;
    when init_rd =>
            next_state_top_eng_seq <= wait0;
    when init_rd_eop =>
            next_state_top_eng_seq <= wait0;
    when wait0 =>
        next_state_top_eng_seq <= init_trb_engine;
  when init_trb_engine =>
        next_state_top_eng_seq <= go_trb_engine;
    when go_trb_engine =>
        if rd_eq_stop_top='1' then
            next_state_top_eng_seq <= idle;
        else
            next_state_top_eng_seq <= go_trb_engine;
        end if;
    -- coverage off
    when others => next_state_top_eng_seq <= idle;
    -- coverage on
    end case;
    
end process FSM_top_eng_seq;

-------------------

FSM_bot_eng_seq: process(state_bot_eng_seq, state_top_eng_seq, rd_eq_stop_top, state_trb, lf_status_addr_top_bits_ptr(5 downto 4),
                         wr_eg_start, rd_eq_stop_bot, lf_status_eop_ptr(1), lf_status_addr_bot_bits_ptr(5 downto 4), survready) 


begin
    case state_bot_eng_seq is

    when idle =>
        if lf_status_eop_ptr(1)='0' and state_trb=fill and (state_top_eng_seq/=idle or (state_top_eng_seq=idle and lf_status_addr_top_bits_ptr(5 downto 4)/="00")) and 
           state_top_eng_seq/=init_rd_eop and lf_status_addr_bot_bits_ptr(5 downto 4)="00" then
            next_state_bot_eng_seq <= init_rd_eop;
        elsif wr_eg_start='1' and    lf_status_eop_ptr(1)='1' and state_trb=fill and state_top_eng_seq/=idle and survready='1' then
        next_state_bot_eng_seq <= init_rd;
        else
            next_state_bot_eng_seq <= idle;
        end if;

    when init_rd =>
        next_state_bot_eng_seq <= wait0;
    when init_rd_eop =>
            next_state_bot_eng_seq <= wait0;
  when wait0 =>
        next_state_bot_eng_seq <= init_trb_engine;
  when init_trb_engine =>
        next_state_bot_eng_seq <= go_trb_engine;
    when go_trb_engine =>
        if rd_eq_stop_bot='1' then
            next_state_bot_eng_seq <= idle;
        else
            next_state_bot_eng_seq <= go_trb_engine;
        end if;
    -- coverage off
    when others => next_state_bot_eng_seq <= idle;
    -- coverage on
    end case;
    
end process FSM_bot_eng_seq;


FSM_read_top_bits: process(state_read_top_bits, state_read_bot_bits, chunk_top_ready, lf_job_start, --(1)(1), lf_job_start(2)(1), 
                                                        rd_eq_stop_top_bits, rd_eq_stop_bot_bits, lf_status_addr_top_bits_ptr(2 downto 1), ena_trb_pu)


begin
    case state_read_top_bits is

    when idle => 
    -- I have to load if chunk_top_ready or if there is data available in the FIFO
        if chunk_top_ready='1' then
        next_state_read_top_bits <= load;
        else
            next_state_read_top_bits <= idle;
        end if;
    -- here I make the decission to go forward if it in the right order
    when load => 
       if (lf_job_start(1)(1)='1' and state_read_bot_bits/=working) or 
           (lf_job_start(2)(1)='1' and rd_eq_stop_bot_bits='1' and state_read_bot_bits=working and ena_trb_pu='1') then
            next_state_read_top_bits <= working;
        else
           next_state_read_top_bits <= loaded;
        end if;
    when loaded => 
        if (lf_job_start(1)(1)='1' and state_read_bot_bits/=working) or 
           (lf_job_start(2)(1)='1' and rd_eq_stop_bot_bits='1' and state_read_bot_bits=working and ena_trb_pu='1') then
            next_state_read_top_bits <= working;
        else
            next_state_read_top_bits <= loaded;
        end if;
    when working => 
        if (chunk_top_ready='1' or lf_status_addr_top_bits_ptr(2 downto 1)="00") and rd_eq_stop_top_bits='1' and ena_trb_pu='1' then
            next_state_read_top_bits <= load;
        elsif chunk_top_ready='0' and lf_status_addr_top_bits_ptr(2 downto 1)="10" and rd_eq_stop_top_bits='1' and ena_trb_pu='1' then
            next_state_read_top_bits <= idle;
        else
            next_state_read_top_bits <= working;
        end if;
    -- coverage off
    when others => next_state_read_top_bits <= idle;
    -- coverage on
    end case;
    
end process FSM_read_top_bits;


FSM_read_bot_bits: process(state_read_bot_bits, state_read_top_bits, chunk_bot_ready, lf_job_start, --(1)(1), lf_job_start(2)(1),
                                                        rd_eq_stop_bot_bits, rd_eq_stop_top_bits, lf_status_addr_bot_bits_ptr(2 downto 1), ena_trb_pu)

begin
    case state_read_bot_bits is

    when idle => 
        if chunk_bot_ready='1' then
        next_state_read_bot_bits <= load;
        else
            next_state_read_bot_bits <= idle;
        end if;
    when load => 
        if (lf_job_start(1)(1)='0' and state_read_top_bits/=working) or 
           (lf_job_start(2)(1)='0' and rd_eq_stop_top_bits='1' and state_read_top_bits=working and ena_trb_pu='1') then
            next_state_read_bot_bits <= working;
        else
           next_state_read_bot_bits <= loaded;
        end if;
    when loaded => 
        if (lf_job_start(1)(1)='0' and state_read_top_bits/=working) or 
           (lf_job_start(2)(1)='0' and rd_eq_stop_top_bits='1' and state_read_top_bits=working and ena_trb_pu='1') then
            next_state_read_bot_bits <= working;
        else
            next_state_read_bot_bits <= loaded;
        end if;
    when working => 
    -- OK review the load conditions
        if (chunk_bot_ready='1' or lf_status_addr_bot_bits_ptr(2 downto 1)="00") and rd_eq_stop_bot_bits='1'  and ena_trb_pu='1' then
            next_state_read_bot_bits <= load;
        elsif chunk_bot_ready='0' and lf_status_addr_bot_bits_ptr(2 downto 1)="10" and rd_eq_stop_bot_bits='1'  and ena_trb_pu='1' then
            next_state_read_bot_bits <= idle;
        else
            next_state_read_bot_bits <= working;
        end if;
    -- coverage off
    when others => next_state_read_bot_bits <= idle;
    -- coverage on
    end case;
    
end process FSM_read_bot_bits;


clk_FSMs: Process (clk, reset)
    begin
        if reset='1' then
                state_trb <= S0;
            state_top_eng_seq <= idle;
            state_bot_eng_seq <= idle;
            state_read_top_bits <= idle;
            state_read_bot_bits <= idle;
        elsif Rising_edge(clk) then
            state_trb <= next_state_trb;
            state_top_eng_seq <= next_state_top_eng_seq;
            state_bot_eng_seq <= next_state_bot_eng_seq;
            state_read_top_bits <= next_state_read_top_bits;
            state_read_bot_bits <= next_state_read_bot_bits;
        end if;
        
end process clk_FSMs;


init_startadd <= out_fsm_trb(1);
inc_startadd_d <= out_fsm_trb(2);
lf_pull_addr_ptr <= out_fsm_trb(3);
init_next_stopadd <= out_fsm_trb(4);

outputs_FSM_trb: process(state_trb)

    begin
        case state_trb is
        when S0 =>
            out_fsm_trb <= "0000";
        when init =>
            out_fsm_trb <= "0001";
        when fill =>
            out_fsm_trb <= "0000";
        when eop_handed =>
            out_fsm_trb <= "1000";
        when wait_new =>
            out_fsm_trb <= "0000";
        when new_block =>
            out_fsm_trb <= "0101";
        when hand_over =>
      out_fsm_trb <= "0010";
        -- coverage off
        when others => 
            out_fsm_trb <= "0000";
        -- coverage on
        end case;
        
end process outputs_FSM_trb;


init_readtop <= out_fsm_top_eng_seq(1);
load_readtop <= out_fsm_top_eng_seq(2);
 ena_readtop <= out_fsm_top_eng_seq(3);
load_top_trb_eng <= out_fsm_top_eng_seq(4);


outputs_FSM_top_eng_seq: process(state_top_eng_seq)

    begin
        case state_top_eng_seq is
        when idle =>
            out_fsm_top_eng_seq <= "0000";
        when wait0 =>
      out_fsm_top_eng_seq <= "0100";
        when init_rd => 
      out_fsm_top_eng_seq <= "0010";
        when init_rd_eop => 
            out_fsm_top_eng_seq <= "0001";
    when init_trb_engine =>
            out_fsm_top_eng_seq <= "1100";
        when go_trb_engine =>
            out_fsm_top_eng_seq <= "0100";
        -- coverage off
        when others => 
            out_fsm_top_eng_seq <= "0000";
        -- coverage on
        end case;
        
end process outputs_FSM_top_eng_seq;


init_readbot <= out_fsm_bot_eng_seq(1);
load_readbot <= out_fsm_bot_eng_seq(2);
 ena_readbot <= out_fsm_bot_eng_seq(3);
load_bot_trb_eng <= out_fsm_bot_eng_seq(4);


outputs_FSM_bot_eng_seq: process(state_bot_eng_seq)

    begin
        case state_bot_eng_seq is
        when idle =>
            out_fsm_bot_eng_seq <= "0000";
        when wait0 =>
      out_fsm_bot_eng_seq <= "0100";
        when init_rd => 
      out_fsm_bot_eng_seq <= "0010";
        when init_rd_eop => 
            out_fsm_bot_eng_seq <= "0001";
    when init_trb_engine =>
            out_fsm_bot_eng_seq <= "1100";
        when go_trb_engine =>
            out_fsm_bot_eng_seq <= "0100";
        -- coverage off
        when others => 
            out_fsm_bot_eng_seq <= "0000";
        -- coverage on
        end case;
        
end process outputs_FSM_bot_eng_seq;
   

-- REMINDER: If logic FIFOs are re-sized then in here the coefficients have to be changed in signals
--  lf_status_...
outputs_FSM_ena_assert_ctrl: process(lf_status_addr_ptr(3), symbol_count, lf_status_job_start(7 downto 6), 
                                                                         lf_status_addr_top_bits_ptr(5 downto 4), lf_status_addr_bot_bits_ptr(5 downto 4), 
                                                                         lf_status_sop_ptr(4), lf_status_eop_ptr(4)) 

    begin
        if (unsigned(symbol_count) > unsigned(count_limit)) then --or (state_bot_eng_seq=init_rd_wait and state_top_eng_seq/=go_trb_engine) or
         --(state_top_eng_seq=init_rd_wait and state_bot_eng_seq/=go_trb_engine) then
             allow_ena_assert_int <= '0';
        -- REMINDER: If logic FIFOs are re-sized then in here the coefficients have to be changed
        elsif lf_status_addr_ptr(3)='1' or lf_status_job_start(6)='1' or lf_status_job_start(7)='1' or
                    lf_status_addr_top_bits_ptr(4)='1' or lf_status_addr_top_bits_ptr(5)='1' or lf_status_addr_bot_bits_ptr(4)='1' or 
                    lf_status_addr_bot_bits_ptr(5)='1' or lf_status_sop_ptr(4)='1' or    lf_status_eop_ptr(4)='1' then 
            allow_ena_assert_int <= '0';
        else
            allow_ena_assert_int <= '1';
        end if;
        
end process outputs_FSM_ena_assert_ctrl;

-- I need to enlarge ena_read asserted if ena_trb_pu stays de-asserted and
--  ena_read_top_bits changes
dav_top_eng <= ena_read_top_bits; 
dav_bot_eng <= ena_read_bot_bits; 


outputs_FSM_read_top_bits: process(state_read_top_bits, rd_eq_stop_top_bits, ena_trb_pu)

    begin
        case state_read_top_bits is
        when idle =>
            ena_read_top_bits <= '0';
            load_read_top_bits <= '0';
            lf_pull_addr_top_bits_ptr <= '0';
        when load =>
            ena_read_top_bits <= '0';
            load_read_top_bits <= '1';
            lf_pull_addr_top_bits_ptr <= '0';
        when loaded =>
            ena_read_top_bits <= '0';
            load_read_top_bits <= '0';
            lf_pull_addr_top_bits_ptr <= '0';
        when working =>
            ena_read_top_bits <= '1';
            load_read_top_bits <= '0';
            if rd_eq_stop_top_bits='1' and ena_trb_pu='1' then
                lf_pull_addr_top_bits_ptr <= '1';
            else
                lf_pull_addr_top_bits_ptr <= '0';
            end if;
        -- coverage off
        when others => 
            ena_read_top_bits <= '0';
            load_read_top_bits <= '0';
            lf_pull_addr_top_bits_ptr <= '0';
        -- coverage on
        end case;
        
end process outputs_FSM_read_top_bits;


outputs_FSM_read_bot_bits: process(state_read_bot_bits, rd_eq_stop_bot_bits, ena_trb_pu)

    begin
        case state_read_bot_bits is
        when idle =>
            ena_read_bot_bits <= '0';
            load_read_bot_bits <= '0';
            lf_pull_addr_bot_bits_ptr <= '0';
        when load =>
            ena_read_bot_bits <= '0';
            load_read_bot_bits <= '1';
            lf_pull_addr_bot_bits_ptr <= '0';
        when loaded =>
            ena_read_bot_bits <= '0';
            load_read_bot_bits <= '0';
            lf_pull_addr_bot_bits_ptr <= '0';
        when working =>
            ena_read_bot_bits <= '1';
            load_read_bot_bits <= '0';
            if rd_eq_stop_bot_bits='1' and ena_trb_pu='1' then
                lf_pull_addr_bot_bits_ptr <= '1';
            else
                lf_pull_addr_bot_bits_ptr <= '0';
            end if;
        -- coverage off
        when others => 
            ena_read_bot_bits <= '0';
            load_read_bot_bits <= '0';
            lf_pull_addr_bot_bits_ptr <= '0';
        -- coverage on
        end case;
        
end process outputs_FSM_read_bot_bits;


-- I have to look at latch_best_mark: 
-- it would have to be replaced or generated from bmp
wr_fifo <= (sink_eop_del and not sink_eop_del_del);

--  Logic fifo here to replace address_pointer_out
-- in principle this fifo will work outside enable scope 

lf_push_addr_ptr <= wr_fifo;
address_pointer_out <= lf_addr_ptr(1);

addr_ptr_logic_fifo : Process (clk, reset)
begin
if reset='1' then
    lf_addr_ptr <= (others => (others => '0'));
    lf_status_addr_ptr <= "001";
elsif Rising_edge(clk) then
    if lf_push_addr_ptr='1' and lf_pull_addr_ptr='0' and lf_status_addr_ptr(3)='0' then 
        lf_status_addr_ptr(3 downto 2) <= lf_status_addr_ptr(2 downto 1);
        lf_status_addr_ptr(1) <= '0';
    elsif lf_push_addr_ptr='0' and lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_status_addr_ptr(2 downto 1) <= lf_status_addr_ptr(3 downto 2);
        lf_status_addr_ptr(3) <= '0';
        -- what if lf_push_addr_ptr and lf_pull_addr_ptr collide? lf_status_addr_ptr stays the same.
    end if;
    
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(1)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(1) <= writeadd_del;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(1) <= lf_addr_ptr(2);
    end if;
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(3)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(2) <= writeadd_del;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(2) <= (others => '0'); --lf_addr_ptr(3);
    end if;
    -- if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(3)='1' and lf_pull_addr_ptr='0') or
        -- (lf_push_addr_ptr='1' and lf_status_addr_ptr(4)='1' and lf_pull_addr_ptr='1')) then
        -- lf_addr_ptr(3) <= writeadd_del;
    -- elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        -- lf_addr_ptr(3) <= (others => '0');
    -- end if;
    
end if;
end process addr_ptr_logic_fifo;

-- this doesn't work when it has to do 2 reads of the same engine
-- Question: Shall I use load or the end of the job before pulling the job_start fifo??
lf_pull_job_start_gen: Process(rd_eq_stop_top_bits, rd_eq_stop_bot_bits, lf_job_start, --(1)(1), lf_job_start(1)(2),
                                                                state_read_bot_bits, state_read_top_bits, ena_trb_pu)
begin
    if ((rd_eq_stop_top_bits='1' and lf_job_start(1)(1)='1' and state_read_top_bits=working) or
       (rd_eq_stop_bot_bits='1' and lf_job_start(1)(1)='0' and state_read_bot_bits=working)) and ena_trb_pu='1' then
         lf_pull_job_start <= '1';
         eop_source_gen <= lf_job_start(1)(2);
    else
        lf_pull_job_start <= '0';
        eop_source_gen <= '0';
    end if;
end process lf_pull_job_start_gen;
lf_push_job_start <= init_readtop or load_readtop or init_readbot or load_readbot;

job_start_logic_fifo : Process (clk, reset)
begin
if reset='1' then
    lf_job_start <= (others => (others => '0'));
    lf_status_job_start <= "0000001";
elsif Rising_edge(clk) then
    if lf_push_job_start='1' and lf_pull_job_start='0' and lf_status_job_start(7)='0' then 
        lf_status_job_start(7 downto 2) <= lf_status_job_start(6 downto 1);
        lf_status_job_start(1) <= '0';
    elsif lf_push_job_start='0' and lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_status_job_start(6 downto 1) <= lf_status_job_start(7 downto 2);
        lf_status_job_start(7) <= '0';
    end if;
    
    if ((lf_push_job_start='1' and lf_status_job_start(1)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(2)='1' and lf_pull_job_start='1')) then
        lf_job_start(1)(1) <= which_eng_use; -- the 2 bits which(1) and eop (2);
        lf_job_start(1)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(1) <= lf_job_start(2);
    end if;
    if ((lf_push_job_start='1' and lf_status_job_start(2)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(3)='1' and lf_pull_job_start='1')) then
        lf_job_start(2)(1) <= which_eng_use; 
        lf_job_start(2)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(2) <= lf_job_start(3);
    end if;
    if ((lf_push_job_start='1' and lf_status_job_start(3)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(4)='1' and lf_pull_job_start='1')) then
        lf_job_start(3)(1) <= which_eng_use; 
        lf_job_start(3)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(3) <= lf_job_start(4);
    end if;
    if ((lf_push_job_start='1' and lf_status_job_start(4)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(5)='1' and lf_pull_job_start='1')) then
        lf_job_start(4)(1) <= which_eng_use; 
        lf_job_start(4)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(4) <= lf_job_start(5);
    end if;
    if ((lf_push_job_start='1' and lf_status_job_start(5)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(6)='1' and lf_pull_job_start='1')) then
        lf_job_start(5)(1) <= which_eng_use; 
        lf_job_start(5)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(5) <= lf_job_start(6);
    end if;
    if ((lf_push_job_start='1' and lf_status_job_start(6)='1' and lf_pull_job_start='0') or
        (lf_push_job_start='1' and lf_status_job_start(7)='1' and lf_pull_job_start='1')) then
        lf_job_start(6)(1) <= which_eng_use; 
        lf_job_start(6)(2) <= lf_pull_eop_ptr;
    elsif lf_pull_job_start='1' and lf_status_job_start(1)='0' then
        lf_job_start(6) <= (others => '0');
    end if;
    
end if;
end process job_start_logic_fifo;


-- end logic fifo
------------------
      
    stop_RAM: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => MAXSTATES, WIDTH_ADDR => log2_4v, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 0, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => survive, 
      address_a => writeadd, address_b => readtop, 
      clock0 => clk, clock1 => clk,
      q_b => stop ); 


    sbot_RAM: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => MAXSTATES, WIDTH_ADDR => log2_4v, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 0, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => survive, 
      address_a => writeadd, address_b => readbot, 
      clock0 => clk, clock1 => clk,
      q_b => sbot );
      
    best_state_ram: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => L-1, WIDTH_ADDR => log2_4v, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 0, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => baddready, data_a => bestadd_int, 
      address_a => writeadd_del, address_b => readadd, 
      clock0 => clk, clock1 => clk,
      q_b => bestadd_out );
      
    swap_bits_top: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => 1, WIDTH_ADDR => log2_4v, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => valid_top_bits(2), data_a => decbittop_int, 
      address_a => write_top_bits, address_b => read_top_bits, 
      clock0 => clk, clock1 => clk, clocken1 => ena_trb_pu,
      q_b => decbittop_swap );
      
    swap_bits_bot: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => 1, WIDTH_ADDR => log2_4v, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => valid_bot_bits(2), data_a => decbitbot_int, 
      address_a => write_bot_bits, address_b => read_bot_bits, 
      clock0 => clk, clock1 => clk, clocken1 => ena_trb_pu,
      q_b => decbitbot_swap );
      

lf_push_addr_bot_bits_ptr <= chunk_bot_ready; 
lf_push_addr_top_bits_ptr <= chunk_top_ready;
-- this sink_sop_q turns out to be a problem ... Too wide for use as push signal
lf_push_sop_ptr <= sink_sop_q and not sink_sop_q_del;
lf_pull_sop_ptr <= init_next_stopadd; 
lf_pull_eop_ptr <= init_readtop or init_readbot;
lf_push_eop_ptr <= wr_fifo;
 

addr_topbot_bits_ptr_logic_fifo : Process (clk, reset)
begin
if reset='1' then
    lf_addr_bot_bits_ptr <= (others => (others => '0'));
    lf_addr_top_bits_ptr <= (others => (others => '0'));
    stop_bits_top <= (others => '0');
    stop_bits_bot <= (others => '0');
    lf_status_addr_bot_bits_ptr <= "00001";
    lf_status_addr_top_bits_ptr <= "00001";
    lf_status_sop_ptr <= "0001";
    lf_status_eop_ptr <= "0001";
elsif Rising_edge(clk) then
    if lf_push_sop_ptr='1' and lf_pull_sop_ptr='0' and lf_status_sop_ptr(4)='0' then 
        lf_status_sop_ptr(4 downto 2) <= lf_status_sop_ptr(3 downto 1);
        lf_status_sop_ptr(1) <= '0';
    elsif lf_push_sop_ptr='0' and lf_pull_sop_ptr='1' and lf_status_sop_ptr(1)='0' then
        lf_status_sop_ptr(3 downto 1) <= lf_status_sop_ptr(4 downto 2);
        lf_status_sop_ptr(4) <= '0';
    end if;
    if lf_push_eop_ptr='1' and lf_pull_eop_ptr='0' and lf_status_eop_ptr(4)='0' then 
        lf_status_eop_ptr(4 downto 2) <= lf_status_eop_ptr(3 downto 1);
        lf_status_eop_ptr(1) <= '0';
    elsif lf_push_eop_ptr='0' and lf_pull_eop_ptr='1' and lf_status_eop_ptr(1)='0' then
        lf_status_eop_ptr(3 downto 1) <= lf_status_eop_ptr(4 downto 2);
        lf_status_eop_ptr(4) <= '0';
    end if;

    if lf_push_addr_bot_bits_ptr='1' and lf_pull_addr_bot_bits_ptr='0' and lf_status_addr_bot_bits_ptr(5)='0' then 
        lf_status_addr_bot_bits_ptr(5 downto 2) <= lf_status_addr_bot_bits_ptr(4 downto 1);
        lf_status_addr_bot_bits_ptr(1) <= '0';
    elsif lf_push_addr_bot_bits_ptr='0' and lf_pull_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='0' then
        lf_status_addr_bot_bits_ptr(4 downto 1) <= lf_status_addr_bot_bits_ptr(5 downto 2);
        lf_status_addr_bot_bits_ptr(5) <= '0';
    end if;
    if lf_push_addr_top_bits_ptr='1' and lf_pull_addr_top_bits_ptr='0' and lf_status_addr_top_bits_ptr(5)='0' then 
        lf_status_addr_top_bits_ptr(5 downto 2) <= lf_status_addr_top_bits_ptr(4 downto 1);
        lf_status_addr_top_bits_ptr(1) <= '0';
    elsif lf_push_addr_top_bits_ptr='0' and lf_pull_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='0' then
        lf_status_addr_top_bits_ptr(4 downto 1) <= lf_status_addr_top_bits_ptr(5 downto 2);
        lf_status_addr_top_bits_ptr(5) <= '0';
    end if;
    
    if ((lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='1' and lf_pull_addr_bot_bits_ptr='0') or
        (lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(2)='1' and lf_pull_addr_bot_bits_ptr='1')) then
        lf_addr_bot_bits_ptr(1) <= write_bot_bits;
    elsif lf_pull_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='0' then
        lf_addr_bot_bits_ptr(1) <= lf_addr_bot_bits_ptr(2);
    end if;
    if ((lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(2)='1' and lf_pull_addr_bot_bits_ptr='0') or
        (lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(3)='1' and lf_pull_addr_bot_bits_ptr='1')) then
        lf_addr_bot_bits_ptr(2) <= write_bot_bits;
    elsif lf_pull_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='0' then
        lf_addr_bot_bits_ptr(2) <= lf_addr_bot_bits_ptr(3);
    end if;
    if ((lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(3)='1' and lf_pull_addr_bot_bits_ptr='0') or
        (lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(4)='1' and lf_pull_addr_bot_bits_ptr='1')) then
        lf_addr_bot_bits_ptr(3) <= write_bot_bits;
    elsif lf_pull_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='0' then
        lf_addr_bot_bits_ptr(3) <= lf_addr_bot_bits_ptr(4);
    end if;
    if ((lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(4)='1' and lf_pull_addr_bot_bits_ptr='0') or
        (lf_push_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(5)='1' and lf_pull_addr_bot_bits_ptr='1')) then
        lf_addr_bot_bits_ptr(4) <= write_bot_bits;
    elsif lf_pull_addr_bot_bits_ptr='1' and lf_status_addr_bot_bits_ptr(1)='0' then
        lf_addr_bot_bits_ptr(4) <= (others => '0');
    end if;
    
    if ((lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='1' and lf_pull_addr_top_bits_ptr='0') or
        (lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(2)='1' and lf_pull_addr_top_bits_ptr='1')) then
        lf_addr_top_bits_ptr(1) <= write_top_bits;
    elsif lf_pull_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='0' then
        lf_addr_top_bits_ptr(1) <= lf_addr_top_bits_ptr(2);
    end if;
    if ((lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(2)='1' and lf_pull_addr_top_bits_ptr='0') or
        (lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(3)='1' and lf_pull_addr_top_bits_ptr='1')) then
        lf_addr_top_bits_ptr(2) <= write_top_bits;
    elsif lf_pull_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='0' then
        lf_addr_top_bits_ptr(2) <= lf_addr_top_bits_ptr(3);
    end if;
    if ((lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(3)='1' and lf_pull_addr_top_bits_ptr='0') or
        (lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(4)='1' and lf_pull_addr_top_bits_ptr='1')) then
        lf_addr_top_bits_ptr(3) <= write_top_bits;
    elsif lf_pull_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='0' then
        lf_addr_top_bits_ptr(3) <= lf_addr_top_bits_ptr(4);
    end if;
    if ((lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(4)='1' and lf_pull_addr_top_bits_ptr='0') or
        (lf_push_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(5)='1' and lf_pull_addr_top_bits_ptr='1')) then
        lf_addr_top_bits_ptr(4) <= write_top_bits;
    elsif lf_pull_addr_top_bits_ptr='1' and lf_status_addr_top_bits_ptr(1)='0' then
        lf_addr_top_bits_ptr(4) <= (others => '0');
    end if;
    
    if lf_pull_addr_top_bits_ptr='1' then
        stop_bits_top <= unsigned(lf_addr_top_bits_ptr(1)) + natural(1);
    end if;
    if lf_pull_addr_bot_bits_ptr='1' then
        stop_bits_bot <= unsigned(lf_addr_bot_bits_ptr(1)) + natural(1);
    end if;
    
end if;
end process addr_topbot_bits_ptr_logic_fifo;


ifg56: if ncodes>1 generate
bestadd_int <= bestadd;
end generate ifg56;

ifg57: if ncodes=1 generate
    muxbest: process(tr_init_state, bestadd, tb_type, sink_eop_del)
    
    begin
        if tb_type='1' and sink_eop_del='1' then
            bestadd_int <= tr_init_state;
        else
            bestadd_int <= bestadd;
        end if;
    end process muxbest;
end generate ifg57;


decoding: auk_vit_sel
    generic map ( inwidth => L-1, outwidth => maxstates)
    port map (selin => bestadd_out,    selout => statewide);

fg4: for k in 1 to halfstates generate
  tracetop_d(2*k-1) <= ((tracetop_q(k) and not stop(k)) or
                                               (tracetop_q(k+halfstates) and not stop(k+halfstates)));
  tracetop_d(2*k) <= ((tracetop_q(k) and stop(k)) or
                                       (tracetop_q(k+halfstates) and stop(k+halfstates)));
    tracebot_d(2*k-1) <= ((tracebot_q(k) and not sbot(k)) or
                                                 (tracebot_q(k+halfstates) and not sbot(k+halfstates)));
  tracebot_d(2*k) <= ((tracebot_q(k) and sbot(k)) or
                                          (tracebot_q(k+halfstates) and sbot(k+halfstates)));
end generate fg4;

tracetop : Process(clk, reset)
begin
if reset='1' then
  tracetop_q <= one_maxstates;
elsif Rising_edge(clk) then
    if load_top_trb_eng='1' then
    tracetop_q <= statewide;
    else --if ena_trb_pu='1' then
    tracetop_q <= tracetop_d;
  end if;
end if;
end process tracetop;          

tracebot : Process(clk, reset)
begin
if reset='1' then
  tracebot_q <= one_maxstates;
elsif Rising_edge(clk) then
    if load_bot_trb_eng='1' then
    tracebot_q <= statewide;
    else --if ena_trb_pu='1' then
    tracebot_q <= tracebot_d;
  end if;
end if;
end process tracebot;


decode: process(tracetop_q, tracebot_q)

  variable dectop, decbot : Std_Logic_Vector(halfstates downto 1);
    
begin
    dectop(1) := tracetop_q(halfstates+1);
    decbot(1) := tracebot_q(halfstates+1);
  fg3: FOR k IN 2 TO halfstates loop
    dectop(k) := dectop(k-1) or tracetop_q(halfstates+k);
        decbot(k) := decbot(k-1) or tracebot_q(halfstates+k);
  end loop fg3;
    decbittop_int(1) <= dectop(halfstates);
    decbitbot_int(1) <= decbot(halfstates);
  
end process decode;

--------

allow_ena_assert <= allow_ena_assert_int; -- and ena_trb_pu;

-------  

dav_source_int <= dav_bot_eng_del(2) or dav_top_eng_del(2);

sop_source_gen <= (dav_top_eng or dav_bot_eng) and allow_next_sop;

mux_data_int : process(dav_top_eng_del(2), dav_bot_eng_del(2), decbittop_swap(1), decbitbot_swap(1))
begin
    if dav_top_eng_del(2)='1' and dav_bot_eng_del(2)='0' then
        dat_source_int_d <= decbittop_swap(1);
    elsif dav_top_eng_del(2)='0' and dav_bot_eng_del(2)='1' then
    dat_source_int_d <= decbitbot_swap(1);
    else
        dat_source_int_d <= '0';
    end if;    
end process mux_data_int;

pipe_delay : Process (clk, reset)

    variable decbit_d, sop_source_d, eop_source_d : Std_Logic;
    variable data_val_d : Std_Logic;
  
begin
if reset = '1' then
    --decbit_q <= '0';
    --decbit_shunt <= '0';
    --data_val_shunt <= '0';
    --val_source_q <= '0';
    allow_next_sop <= '1';
    --sop_source_shunt <= '0';
  --eop_source_shunt <= '0';
    dav_bot_eng_del <= (others => '0');
    dav_top_eng_del <= (others => '0');
    --data_val_pipe(3) <= '0';
    eop_source_pipe(2 downto 1) <= (others => '0');
    sop_source_pipe(2 downto 1) <= (others => '0');
elsif Rising_edge(clk) then

  -- if ena_trb_pu='1' then
        -- decbit_d := dat_source_int_d; --dat_source_pipe(2); 
        -- data_val_d := dav_source_int; --data_val_pipe(2);
        -- eop_source_d := eop_source_pipe(4);
        -- sop_source_d := sop_source_pipe(4);
    -- else
        -- data_val_d := data_val_shunt;
-- 
        -- decbit_d := decbit_shunt;
        -- eop_source_d := eop_source_shunt;
        -- sop_source_d := sop_source_shunt;
    -- end if;
  if ena_trb_pu='1' then
        dav_bot_eng_del(1) <= dav_bot_eng;
        dav_top_eng_del(1) <= dav_top_eng;
        dav_bot_eng_del(2) <= dav_bot_eng_del(1);
        dav_top_eng_del(2) <= dav_top_eng_del(1);
        eop_source_pipe(1) <= eop_source_gen; 
        sop_source_pipe(1) <= (dav_top_eng or dav_bot_eng) and allow_next_sop;
        eop_source_pipe(2) <= eop_source_pipe(1);
        sop_source_pipe(2) <= sop_source_pipe(1);
        
        -- data_val_shunt <= dav_source_int; 
        
-- 
        -- decbit_shunt <= dat_source_int_d; 
        -- eop_source_shunt <= eop_source_pipe(4);
        -- sop_source_shunt <= sop_source_pipe(4);
  end if;
    -- if ena_slave_source='1' then
        -- decbit_q <= decbit_d;
        -- data_val_pipe(3) <= data_val_d;
        -- eop_source_pipe(5) <= eop_source_d;
        -- sop_source_pipe(5) <= sop_source_d;
    -- end if;

  --val_source_q <= ena_slave_source and allow_val_assert;

    --  Alex 22-01-2007  to remain here
    if ena_trb_pu='1' then
        if eop_source_gen='1' then 
            allow_next_sop <= '1';
        elsif sop_source_gen='1' then
            allow_next_sop <= '0';
        end if;
    end if;
        
end if;
end process pipe_delay;

data_available <= dav_source_int;
decbit <= dat_source_int_d; --decbit_q;
--val_source <= source_val_int;
-- Alex 22-0-2007 If I am going to move out this source_val into a separate entity it still needs to come into here as it is used  
--source_val_int <= val_source_q and data_val_pipe(3);
eop_source <= eop_source_pipe(2); -- eop_source_gen
sop_source <= sop_source_pipe(2); --sop_source_gen
    
end architecture rtl_mem;


Architecture rtl_cnt of auk_vit_par_trb_atl is

-- 18-01-2007   I have to eliminate all  natural_m!! 
-- Example : 
--Constant omegacmp : std_logic_vector(wide downto 1) := 
        --CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => errs-1, SIZE => wide), SIZE => wide);

    
Constant VCC : Std_Logic := '1';
Constant maxstates : NATURAL := 2**(L-1);
Constant halfstates : NATURAL := 2**(L-2);
Constant log2_4v : NATURAL := LOG2_ceil_table(4*v+1);
--Constant config : STRING := "std_no_max_info";
-- Alex 18-01-2007  After the information that Neil gave me, I have to check NumWords usage 
-- to minimize whenever possible the amount of M4K being used  Here and in all other memory instantiations
--Constant NumWords : natural := 2**log2_4v;
Constant NumWords : natural := 4*v;
Constant vector_one : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 1, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => log2_4v), SIZE => log2_4v);
Constant one_maxstates : Std_Logic_Vector(maxstates downto 1) := --natural_2_m(arg => 1, size => maxstates);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => maxstates), SIZE => maxstates);
Constant cnt_modulus_2v : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 2*v-1, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2*v-1, SIZE => log2_4v), SIZE => log2_4v);
Constant cnt_modulus_2v_bis : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 2*v+1, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2*v+1, SIZE => log2_4v), SIZE => log2_4v);

Constant one_v    : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => v, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => v, SIZE => log2_4v), SIZE => log2_4v);
Constant thr_v    : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 3*v, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 3*v, SIZE => log2_4v), SIZE => log2_4v);
Constant for_v_m1 : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 4*v-1, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 4*v-1, SIZE => log2_4v), SIZE => log2_4v);
Constant for_v_p2 : Std_Logic_Vector(log2_4v downto 1) := --natural_2_m(arg => 4*v+2, size => log2_4v);
   CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 4*v+2, SIZE => log2_4v), SIZE => log2_4v);


signal readtop, writetop : Std_Logic_Vector(log2_4v downto 1);
signal readbot, writebot : Std_Logic_Vector(log2_4v downto 1);
signal stoadd, decadd : Std_Logic_Vector(log2_4v downto 1);
signal stadd, outchk, outchk_cnt : Std_Logic_Vector(log2_4v downto 1);
signal tracetop_q, tracetop_d, tracebot_q, tracebot_d : Std_Logic_Vector(maxstates downto 1);
signal data_ram_in : Std_Logic_Vector(maxstates downto 1);
signal stop, sbot : Std_Logic_Vector(maxstates downto 1);
signal dectop, decbot : Std_Logic_Vector(halfstates downto 1);
signal bit_ram_in, bit_ram_out : Std_Logic_Vector(1 downto 1);
signal VCC_signal, decbittop_int, decbitbot_int, decbitmux : Std_Logic;
signal stogte_int, outvalidnode_q, flip_flop_t_out, toggle : Std_Logic;
Signal outvalid : std_logic;
signal full_status_q : std_logic;
signal stadd_ge_for_v : std_logic;
signal enable_int : std_logic;
signal lf_pull_enable_ptr, lf_push_enable_ptr : std_logic;
signal lf_status_enable_ptr : std_logic_vector(6 downto 1);


begin

VCC_signal <= VCC;

-- to be connected
--enable_int <= '1';
--enable_int <= enable_source;
-- to be connected
--allow_ena_assert <= '1';


--ifg22 : if config="std_no_max_info" generate
  --outchk_cnt <= for_v_p2;
    outchk_cnt <= for_v_m1;
--end generate ifg22;

-- Alex 02-02 - 2007  stop the enable when data_available is low while full count is been achieved
-- for the first time;
--enable_int <= enable and ((outvalidnode_q and full_status_q) or (survready and not full_status_q));
-- provisionally until I attempt logic fifo
--  Alex , now enable de - asserted when survready is 0 and the enable_fifo is empty
--  
enable_int <= enable and ((survready and lf_status_enable_ptr(1)) or not lf_status_enable_ptr(1));


--ifg_no_max_info2 : if (config="std_no_max_info" or config="ptcm_no_max_info") generate

-- Sync_rot needed? 

stadd_cnt : Process(clk, reset)
begin
if reset='1' then
  stadd <= (others => '0');
elsif Rising_edge(clk) then
    if enable_int='1' then
    --if survready='1' and outvalidnode_q='0' then
        --if outchk(log2_4v)='1' then
        if stadd_ge_for_v='0' then
        stadd <= unsigned(stadd) + natural(1);
        end if;
    --elsif outvalidnode_q='1' and survready='1' and enable='0' then
    --    stadd <= unsigned(stadd) + natural(1);
    --elsif outvalidnode_q='1' and survready='0' and enable='1' then
    --    stadd <= unsigned(stadd) - natural(1);
  end if;
end if;
end process stadd_cnt;          

-- Alex :  23-01-2007  Volker  if you ever read this , this is not my code, this is
--   copycat translation of Martin Langhammer style coding in AHDL !!  ;-)
outchk(1) <= outchk_cnt(1) xor stadd(1);  
fg1: FOR k IN 2 TO log2_4v GENERATE
    outchk(k) <= outchk(k-1) or (outchk_cnt(k) xor stadd(k));
END GENERATE fg1;

comp3: process(stadd)
begin
    --if unsigned(stadd) >= unsigned(for_v_m1) then
    if unsigned(stadd) >= natural(4*v - 2) then
        stadd_ge_for_v <= '1';
    else
        stadd_ge_for_v <= '0';
    end if;
end process comp3;
-- compare with for_v_m1

-- push when survready=1 and enable=0, pull when enable=1 and survready=0
lf_push_enable_ptr <= survready and not enable;
lf_pull_enable_ptr <= not survready and enable;

outvalid_clk: Process (clk, reset)
    begin
        if reset='1' then
            outvalidnode_q <= '0';
            --full_status_q <= '0';
            lf_status_enable_ptr <= "000001";
        elsif Rising_edge(clk) then
        
            if lf_push_enable_ptr='1' and lf_pull_enable_ptr='0' and lf_status_enable_ptr(6)='0' then 
                lf_status_enable_ptr(6 downto 2) <= lf_status_enable_ptr(5 downto 1);
                lf_status_enable_ptr(1) <= '0';
            elsif lf_push_enable_ptr='0' and lf_pull_enable_ptr='1' and lf_status_enable_ptr(1)='0' then
                lf_status_enable_ptr(5 downto 1) <= lf_status_enable_ptr(6 downto 2);
                lf_status_enable_ptr(6) <= '0';
            end if;
            
        
        
            --if enable_int='1' then
            -- Alex 19-01-2007  is this enough to find outvalid or does it have to be stopped by enable as well? 
            -- will this outvalid be sufficient solid as internal valid signal into the atlantic source controller => Find out!! 
            --if survready='1' then
            if enable_int='1' then 
              if outvalidnode_q='0' and stadd_ge_for_v='1' then
                    outvalidnode_q <= '1'; --not outchk(log2_4v);
                --elsif full_status_q='1' then
                    --outvalidnode_q <= stadd_ge_for_v;
                end if;
            end if;
            --if enable='1' and stadd_ge_for_v='1' then
            --    full_status_q <='1';
            --end if;
        end if;
        
end process outvalid_clk;


readtop_cnt : Process(clk, reset)
begin
if reset='1' then
    readtop <= (others => '0');
elsif Rising_edge(clk) then
    if enable_int='1' then
        if unsigned(readtop)=natural(0) then
            readtop <= for_v_m1;
        else
        readtop <= unsigned(readtop) - natural(1);  -- DOWN
        end if;
  end if;
end if;
end process readtop_cnt;          

readbot_cnt : Process(clk, reset)
begin                                 
if reset='1' then
    readbot <= one_v;
elsif Rising_edge(clk) then
    if enable_int='1' then
        if unsigned(readbot)=natural(0) then
            --readbot <= cnt_modulus_4v;
            readbot <= for_v_m1;
        else
        readbot <= unsigned(readbot) - natural(1);  -- DOWN
        end if;
  end if;
end if;
end process readbot_cnt;          


writetop_cnt : Process(clk, reset)
begin
if reset='1' then
  writetop <= (others => '0');
elsif Rising_edge(clk) then
  --if enable_int='1' then
    if survready='1' then
        if writetop=for_v_m1 then
            writetop <= (others => '0');
        else
        writetop <= unsigned(writetop) + natural(1);
        end if;
  end if;
end if;
end process writetop_cnt;          


writebot_cnt : Process(clk, reset)
begin
if reset='1' then
    writebot <= thr_v;
elsif Rising_edge(clk) then
    --if enable_int='1' then
    if survready='1' then
        if writebot=for_v_m1 then
            writebot <= (others => '0');
        else
        writebot <= unsigned(writebot) + natural(1);
        end if;
  end if;
end if;
end process writebot_cnt;

--end generate ifg_no_max_info2;



data_ram_in <= survive;


    stop_RAM: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => MAXSTATES, WIDTH_ADDR => LOG2_4V, NUMWORDS => NumWords,
      INIT_FILE => ini_filename, USE_INIFILE=> 1, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => data_ram_in, 
      address_a => writetop, address_b => readtop, 
      clock0 => clk, clock1 => clk, clocken1 => enable_int,
      q_b => stop );       

    sbot_RAM: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => MAXSTATES, WIDTH_ADDR => LOG2_4V, NUMWORDS => NumWords,
      INIT_FILE => ini_filename, USE_INIFILE=> 1, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => data_ram_in, 
      address_a => writebot, address_b => readbot, 
      clock0 => clk, clock1 => clk, clocken1 => enable_int,
      q_b => sbot ); 


-- traceback the treilli state starting from 0001. 
-- the previous state was 0001 if we kept the lower branch of the current state 0001
-- or if we kept the lower branch of the current state 0100.
-- the previous state was 0010 if we kept the upper branch of the current state 0001
-- or if we kept the upper branch of the current state 0100.
fg4: for k in 1 to halfstates generate
  tracetop_d(2*k-1) <= ((tracetop_q(k) and not stop(k)) or
                                               (tracetop_q(k+halfstates) and not stop(k+halfstates)));
  tracetop_d(2*k) <= ((tracetop_q(k) and stop(k)) or
                                       (tracetop_q(k+halfstates) and stop(k+halfstates)));
    tracebot_d(2*k-1) <= ((tracebot_q(k) and not sbot(k)) or
                                                 (tracebot_q(k+halfstates) and not sbot(k+halfstates)));
  tracebot_d(2*k) <= ((tracebot_q(k) and sbot(k)) or
                                          (tracebot_q(k+halfstates) and sbot(k+halfstates)));
end generate fg4;

tracetop : Process(clk, reset)
begin
if reset='1' then
  tracetop_q <= one_maxstates;
elsif Rising_edge(clk) then
    if enable_int='1' then
    tracetop_q <= tracetop_d;
  end if;
end if;
end process tracetop;          

tracebot : Process(clk, reset)
begin
if reset='1' then
  tracebot_q <= one_maxstates;
elsif Rising_edge(clk) then
    if enable_int='1' then
    tracebot_q <= tracebot_d;
  end if;
end if;
end process tracebot;

-- if we are in the upper part of the treilli the msg bit was a 1
dectop(1) <= tracetop_q(halfstates+1);
decbot(1) <= tracebot_q(halfstates+1);
fg5: for k in 2 to halfstates generate
  dectop(k) <= dectop(k-1) or tracetop_q(halfstates+k);
    decbot(k) <= decbot(k-1) or tracebot_q(halfstates+k);
end generate fg5;

decbittop_int <= dectop(halfstates);
decbitbot_int <= decbot(halfstates);

-- stoadd and decadd sequencers need to be redesign to avoid the overlaping - Done


FF_T : Process (clk, reset)
begin
    if reset='1' then
        flip_flop_t_out <= '0';
    elsif Rising_edge(clk) then
        if enable_int='1' and toggle='1' then
            flip_flop_t_out <= not flip_flop_t_out;
        end if;
    end if;
end process FF_T;

stoadd_cnt : Process(clk, reset)
begin
if reset='1' then
    stoadd <= one_v;
elsif Rising_edge(clk) then
    if enable_int='1' then
        if flip_flop_t_out='0' then
            stoadd <= stoadd + natural(1);
        else
        stoadd <= stoadd - natural(1);
        end if;
  end if;
end if;
end process stoadd_cnt;


seq_ctrl: process(stoadd, flip_flop_t_out)

    variable st_eq_2v, st_eq_1, st_ge_v, st_le_v : Std_Logic;

begin

  if unsigned(stoadd) = unsigned(cnt_modulus_2v) then
      st_eq_2v := '1';
    else
      st_eq_2v := '0';
    end if;
    if unsigned(stoadd) = natural(1) then
      st_eq_1 := '1';
    else
      st_eq_1 := '0';
    end if;
    if unsigned(stoadd) >= unsigned(one_v) then
      st_ge_v := '1';
    else
      st_ge_v := '0';
    end if;
    if unsigned(stoadd) <= unsigned(one_v) then
      st_le_v := '1';
    else
      st_le_v := '0';
    end if;
    if flip_flop_t_out='0' then
        toggle <= st_eq_2v;
        stogte_int <= st_ge_v;
        if st_ge_v='0' then
            decadd <= unsigned(stoadd) + natural(1);
        else
            decadd <= unsigned(cnt_modulus_2v) - unsigned(stoadd);
        end if;
    else
      toggle <= st_eq_1;
        stogte_int <= st_le_v;
        if st_le_v='0' then
            decadd <= unsigned(stoadd) - natural(1);
        else
            decadd <= unsigned(cnt_modulus_2v_bis) - unsigned(stoadd);
        end if;
    end if;
    
end process seq_ctrl;


decbitmux <= (decbittop_int and not stogte_int) or (decbitbot_int and stogte_int);


bit_ram_in(1) <= decbitmux;
decbit <= bit_ram_out(1) and outvalidnode_q;
--outvalid <= outvalidnode;
-- I have to review this!! 
--And now question, when is data_available handed over … changes can only happen 
-- when enable is high and the the number of survready=0 with enable=1 have to be passed 
-- as data_available=0 to equalize data … as long as the enable fifo is empty
--data_available <= outvalidnode_q and enable_int;
data_available <= outvalidnode_q and not (enable and (not survready) and lf_status_enable_ptr(1));

    decbit_RAM: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => 1, WIDTH_ADDR => LOG2_4V, NUMWORDS => NumWords,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => VCC_signal, data_a => bit_ram_in, 
      address_a => stoadd, address_b => decadd, 
      clock0 => clk, clock1 => clk, clocken1 => enable_int, 
      q_b => bit_ram_out ); 
      
   
end architecture rtl_cnt;


Architecture rtl_blk of auk_vit_par_trb_atl is

COMPONENT auk_vit_sel
    generic ( inwidth, outwidth: NATURAL );
    port (
        selin : in Std_Logic_Vector(inwidth downto 1);
        selout : out Std_Logic_Vector(outwidth downto 1)
        );
END COMPONENT;


Constant VCC : Std_Logic := '1';
Constant maxstates : NATURAL := 2**(L-1);
Constant halfstates : NATURAL := 2**(L-2);
-- Constant numwords : NATURAL := V + 17;  -- vlog_wide ought to be based on this value


Subtype vector_vlog is Std_Logic_Vector(vlog_wide downto 1);
type matrix_v is array(NATURAL RANGE <>) of vector_vlog;


------
Signal lf_push_addr_ptr_shunt : std_logic;
Signal lf_push_addr_ptr_shunt2 : std_logic;
Signal sink_eop_del_q : std_logic;
Signal load_trb_eng : std_logic;
Signal wren_bit : std_logic;
Signal trace_decode : std_logic_vector(halfstates downto 1);
Signal decoded_bit_to_swap : std_logic_vector(1 downto 1);
Signal decoded_bit_swapped : std_logic_vector(1 downto 1);
Signal init_trace : std_logic_vector(maxstates downto 1);
Signal trace_engine_d : std_logic_vector(maxstates downto 1);
Signal trace_engine_q : std_logic_vector(maxstates downto 1);
signal survivor_2_trace : std_logic_vector(maxstates downto 1);
Signal surv_to_bit_delay : std_logic_vector(3 downto 1);
signal read_push_to_source_sop : std_logic_vector(3 downto 1);
signal read_pull_to_source_eop : std_logic_vector(2 downto 1);
signal end_count_to_load : std_logic_vector(2 downto 1);
signal read_bit_to_data_val : std_logic_vector(2 downto 1);
Signal write_pointer : std_logic_vector(vlog_wide downto 1);
Signal read_pointer : std_logic_vector(vlog_wide downto 1);
signal write_bit_pointer : std_logic_vector(vlog_wide downto 1);
Signal read_bit_pointer : std_logic_vector(vlog_wide downto 1);
Signal wr_surv_addr : std_logic_vector(vlog_wide downto 1);
Signal rd_surv_addr : std_logic_vector(vlog_wide downto 1);
signal wr_bit_addr : std_logic_vector(vlog_wide downto 1);
Signal rd_bit_addr : std_logic_vector(vlog_wide downto 1);
Signal sclear_read_pointer : std_logic;
signal sclear_read_bit_pointer : std_logic;
Signal sclear_write_bit_pointer : std_logic;
Signal wr_surv_addr_status : std_logic;
Signal rd_surv_addr_status : std_logic;
Signal wr_bit_addr_status : std_logic;
Signal rd_bit_addr_status : std_logic;

-- Logic fifos definitions
signal lf_addr_ptr : matrix_v(1 to 2);
Signal lf_pull_addr_ptr, lf_push_addr_ptr : std_logic;
signal lf_status_addr_ptr : std_logic_vector(3 downto 1);
-- This fifo then can supplant lf_status_ena_wr_bit_cnt if done right
signal lf_addr_wr_ptr : matrix_v(1 to 2);
Signal lf_pull_addr_wr_ptr, lf_push_addr_wr_ptr : std_logic;
signal lf_status_addr_wr_ptr : std_logic_vector(3 downto 1);
--
signal lf_addr_rd_ptr : matrix_v(1 to 2);
Signal lf_pull_addr_rd_ptr, lf_push_addr_rd_ptr : std_logic;
signal lf_status_addr_rd_ptr : std_logic_vector(3 downto 1);

-- enable logic fifos for root counters
Signal lf_pull_ena_rd_surv_cnt, lf_push_ena_rd_surv_cnt : std_logic;
signal lf_status_ena_rd_surv_cnt : std_logic_vector(3 downto 1);
Signal lf_pull_ena_wr_bit_cnt, lf_push_ena_wr_bit_cnt : std_logic;
signal lf_status_ena_wr_bit_cnt : std_logic_vector(3 downto 1);
 
 
begin


------------------------------------
-- Alex 09-02-2007  new design, I want to eliminate tb_length and all related signals
-- use the logic fifo paradigm 



pointers_counters : Process (clk, reset)
begin
if reset = '1' then
    write_pointer <= (others => '0');
    read_pointer <= (others => '0');
    write_bit_pointer <= (others => '0');
    read_bit_pointer <= (others => '0');
    --
    wr_surv_addr_status <= '0';
    rd_surv_addr_status <= '0';
    wr_bit_addr_status <= '0';
    rd_bit_addr_status <= '0';
    
elsif Rising_edge(clk) then
    if survready='1' then
        if sink_eop_del='1' then
            write_pointer <= (others => '0');
        else
            write_pointer <= unsigned(write_pointer) + natural(1); -- down
        end if;
    end if;
    -- there should be a signal to let read go ahead counting
    -- although the fact that lf_addr_ptr(1) is zero is sufficient to hold the start 
    -- at least at the beginning
    if enable='1' then
        if sclear_read_pointer='1' then
            read_pointer <= (others => '0');
        elsif lf_status_ena_rd_surv_cnt(1)='0' then  -- I need an enable here for this counter
            read_pointer <= unsigned(read_pointer) + natural(1); -- down
        end if;
    end if;
    
    if enable='1' then
        if sclear_write_bit_pointer='1' then
            write_bit_pointer <= (others => '0');
        elsif lf_status_ena_wr_bit_cnt(1)='0' then
            write_bit_pointer <= unsigned(write_bit_pointer) + natural(1); -- down
        end if;
    end if;
    
    if enable='1' then
        if sclear_read_bit_pointer='1' then
            read_bit_pointer <= (others => '0');
        elsif lf_status_addr_rd_ptr(1)='0' then
            read_bit_pointer <= unsigned(read_bit_pointer) + natural(1); -- down
        end if;
    end if;
    -- lf_push_addr_ptr may be asserted when enable may be de-asserted!!
    --if enable='1' and lf_push_addr_ptr='1' then
    if lf_push_addr_ptr='1' then
        wr_surv_addr_status <= not wr_surv_addr_status;
    end if;
    if enable='1' and sclear_read_pointer='1' and lf_status_ena_rd_surv_cnt(1)='0' then
        rd_surv_addr_status <= not rd_surv_addr_status;
    end if;
    if enable='1' and sclear_write_bit_pointer='1' and lf_status_ena_wr_bit_cnt(1)='0' then
        wr_bit_addr_status <= not wr_bit_addr_status;
    end if;
    -- this may be toggling if no enable ... I need enable propagation too!!
    -- Alex 12-02-2007 the enable ought to be the status of the logic fifo?
    -- 
    if enable='1' and sclear_read_bit_pointer='1' and lf_status_addr_rd_ptr(1)='0' then
        rd_bit_addr_status <= not rd_bit_addr_status;
    end if;
    
end if;
end process pointers_counters;


delay1 : Process (clk, reset)
begin
if reset='1' then
  lf_push_addr_ptr_shunt <= '0';
    lf_push_addr_ptr_shunt2 <= '0';
    sink_eop_del_q <= '0';
    surv_to_bit_delay <= (others => '0');
    end_count_to_load <= (others => '0');
    read_bit_to_data_val <= (others => '0');
    read_push_to_source_sop <= (others => '0');
    read_pull_to_source_eop <= (others => '0');
elsif Rising_edge(clk) then
    sink_eop_del_q <= sink_eop_del;
    if lf_push_addr_ptr='1' and enable='0' then
        lf_push_addr_ptr_shunt <= '1';
    elsif enable='1' and lf_push_addr_ptr_shunt='1' then
        lf_push_addr_ptr_shunt <= '0';
    end if;
    if lf_push_addr_ptr='1' and lf_status_addr_ptr(1)='1' and enable='0' then
        lf_push_addr_ptr_shunt2 <= '1';
    elsif enable='1' and lf_push_addr_ptr_shunt2='1' then
        lf_push_addr_ptr_shunt2 <= '0';
    end if;
    -- Alex 05-03-07  I need to log  lf_push_addr_ptr properly
    -- considering that enable may be down!!
    if enable='1' then
        surv_to_bit_delay(1) <= lf_push_addr_ptr or lf_push_addr_ptr_shunt; 
        surv_to_bit_delay(3 downto 2) <= surv_to_bit_delay(2 downto 1);
        -- could it be a problem on capturing sink_eop_del with enable toggling??
        -- yes it could be a problem!!
        end_count_to_load(1) <= ((lf_push_addr_ptr and lf_status_addr_ptr(1)) or lf_push_addr_ptr_shunt2) or (sclear_read_pointer and not lf_status_addr_ptr(1));
        end_count_to_load(2) <= end_count_to_load(1);
        --
        read_bit_to_data_val(1) <= not lf_status_addr_rd_ptr(1);
        read_bit_to_data_val(2) <= read_bit_to_data_val(1);
        read_push_to_source_sop(1) <= sclear_write_bit_pointer and not lf_status_ena_wr_bit_cnt(1);
        read_push_to_source_sop(3 downto 2) <= read_push_to_source_sop(2 downto 1);
        read_pull_to_source_eop(1) <= sclear_read_bit_pointer and not lf_status_addr_rd_ptr(1);
        read_pull_to_source_eop(2) <= read_pull_to_source_eop(1);
    end if;
end if;

end process delay1; 

load_trb_eng <= end_count_to_load(2);

lf_push_addr_ptr <= sink_eop_del and not sink_eop_del_q;
lf_pull_addr_ptr <= sclear_read_pointer and not lf_status_ena_rd_surv_cnt(1) and enable;
lf_push_addr_wr_ptr <= lf_push_addr_ptr; --sink_eop_del; 
lf_pull_addr_wr_ptr <= sclear_write_bit_pointer and not lf_status_ena_wr_bit_cnt(1) and enable;
lf_push_addr_rd_ptr <= sclear_write_bit_pointer and not lf_status_ena_wr_bit_cnt(1) and enable;
lf_pull_addr_rd_ptr <= sclear_read_bit_pointer and not lf_status_addr_rd_ptr(1) and enable;

-- Alex 25-03-2007  this is the error, I need to push this fifo in synch with enable while 
-- lf_push_addr_ptr (sink_eop_del) may get asserted when enable is de-asserted, hence I need shunt buffer 
-- to synchronize.
lf_push_ena_rd_surv_cnt <= (lf_push_addr_ptr or lf_push_addr_ptr_shunt) and enable; --sink_eop_del;
lf_pull_ena_rd_surv_cnt <= sclear_read_pointer and not lf_status_ena_rd_surv_cnt(1) and enable;
lf_push_ena_wr_bit_cnt <= surv_to_bit_delay(2) and enable; --sclear_read_pointer; -- there should be a delay ... To be revised
lf_pull_ena_wr_bit_cnt <= sclear_write_bit_pointer and not lf_status_ena_wr_bit_cnt(1) and enable;
----



-- Logic fifos code
addr_ptr_logic_fifo : Process (clk, reset)
begin
if reset='1' then
    lf_addr_ptr <= (others => (others => '0'));
    lf_status_addr_ptr <= "001";
    lf_addr_rd_ptr <= (others => (others => '0'));
    lf_status_addr_rd_ptr <= "001";
    lf_addr_wr_ptr <= (others => (others => '0'));
    lf_status_addr_wr_ptr <= "001";
    lf_status_ena_rd_surv_cnt <= "001";
    lf_status_ena_wr_bit_cnt <= "001";
elsif Rising_edge(clk) then
    if lf_push_addr_ptr='1' and lf_pull_addr_ptr='0' and lf_status_addr_ptr(3)='0' then 
        lf_status_addr_ptr(3 downto 2) <= lf_status_addr_ptr(2 downto 1);
        lf_status_addr_ptr(1) <= '0';
    elsif lf_push_addr_ptr='0' and lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_status_addr_ptr(2 downto 1) <= lf_status_addr_ptr(3 downto 2);
        lf_status_addr_ptr(3) <= '0';
    end if;
    
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(1)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(1) <= write_pointer;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(1) <= lf_addr_ptr(2);
    end if;
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(3)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(2) <= write_pointer;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(2) <= (others => '0'); 
    end if;
    -- next logic fifo
    if lf_push_addr_wr_ptr='1' and lf_pull_addr_wr_ptr='0' and lf_status_addr_wr_ptr(3)='0' then 
        lf_status_addr_wr_ptr(3 downto 2) <= lf_status_addr_wr_ptr(2 downto 1);
        lf_status_addr_wr_ptr(1) <= '0';
    elsif lf_push_addr_wr_ptr='0' and lf_pull_addr_wr_ptr='1' and lf_status_addr_wr_ptr(1)='0' then
        lf_status_addr_wr_ptr(2 downto 1) <= lf_status_addr_wr_ptr(3 downto 2);
        lf_status_addr_wr_ptr(3) <= '0';
    end if;


    --    if ((lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(1)='1' and lf_pull_addr_wr_ptr='0')) then---------------------------------------------------------------------------
    --    lf_addr_wr_ptr(1) <= write_pointer + natural(1);
    --  elsif ((lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(2)='1' and lf_pull_addr_wr_ptr='1')) then
    if ((lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(1)='1' and lf_pull_addr_wr_ptr='0') or
        (lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(2)='1' and lf_pull_addr_wr_ptr='1')) then
        lf_addr_wr_ptr(1) <= write_pointer;
    elsif lf_pull_addr_wr_ptr='1' and lf_status_addr_wr_ptr(1)='0' then
        lf_addr_wr_ptr(1) <= lf_addr_wr_ptr(2);
    end if;
    if ((lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(2)='1' and lf_pull_addr_wr_ptr='0') or
        (lf_push_addr_wr_ptr='1' and lf_status_addr_wr_ptr(3)='1' and lf_pull_addr_wr_ptr='1')) then
        lf_addr_wr_ptr(2) <= write_pointer;
    elsif lf_pull_addr_wr_ptr='1' and lf_status_addr_wr_ptr(1)='0' then
        lf_addr_wr_ptr(2) <= (others => '0'); 
    end if;
    -- next logic fifo
    if lf_push_addr_rd_ptr='1' and lf_pull_addr_rd_ptr='0' and lf_status_addr_rd_ptr(3)='0' then 
        lf_status_addr_rd_ptr(3 downto 2) <= lf_status_addr_rd_ptr(2 downto 1);
        lf_status_addr_rd_ptr(1) <= '0';
    elsif lf_push_addr_rd_ptr='0' and lf_pull_addr_rd_ptr='1' and lf_status_addr_rd_ptr(1)='0' then
        lf_status_addr_rd_ptr(2 downto 1) <= lf_status_addr_rd_ptr(3 downto 2);
        lf_status_addr_rd_ptr(3) <= '0';
    end if;
    
    if ((lf_push_addr_rd_ptr='1' and lf_status_addr_rd_ptr(1)='1' and lf_pull_addr_rd_ptr='0') or
        (lf_push_addr_rd_ptr='1' and lf_status_addr_rd_ptr(2)='1' and lf_pull_addr_rd_ptr='1')) then
        lf_addr_rd_ptr(1) <= lf_addr_wr_ptr(1);
    elsif lf_pull_addr_rd_ptr='1' and lf_status_addr_rd_ptr(1)='0' then
        lf_addr_rd_ptr(1) <= lf_addr_rd_ptr(2);
    end if;
    if ((lf_push_addr_rd_ptr='1' and lf_status_addr_rd_ptr(2)='1' and lf_pull_addr_rd_ptr='0') or
        (lf_push_addr_rd_ptr='1' and lf_status_addr_rd_ptr(3)='1' and lf_pull_addr_rd_ptr='1')) then
        lf_addr_rd_ptr(2) <= lf_addr_wr_ptr(1);
    elsif lf_pull_addr_rd_ptr='1' and lf_status_addr_rd_ptr(1)='0' then
        lf_addr_rd_ptr(2) <= (others => '0'); 
    end if;
    
    -- enable logic fifos
    if lf_push_ena_rd_surv_cnt='1' and lf_pull_ena_rd_surv_cnt='0' and lf_status_ena_rd_surv_cnt(3)='0' then 
        lf_status_ena_rd_surv_cnt(3 downto 2) <= lf_status_ena_rd_surv_cnt(2 downto 1);
        lf_status_ena_rd_surv_cnt(1) <= '0';
    elsif lf_push_ena_rd_surv_cnt='0' and lf_pull_ena_rd_surv_cnt='1' and lf_status_ena_rd_surv_cnt(1)='0' then
        lf_status_ena_rd_surv_cnt(2 downto 1) <= lf_status_ena_rd_surv_cnt(3 downto 2);
        lf_status_ena_rd_surv_cnt(3) <= '0';
    end if;
    if lf_push_ena_wr_bit_cnt='1' and lf_pull_ena_wr_bit_cnt='0' and lf_status_ena_wr_bit_cnt(3)='0' then 
        lf_status_ena_wr_bit_cnt(3 downto 2) <= lf_status_ena_wr_bit_cnt(2 downto 1);
        lf_status_ena_wr_bit_cnt(1) <= '0';
    elsif lf_push_ena_wr_bit_cnt='0' and lf_pull_ena_wr_bit_cnt='1' and lf_status_ena_wr_bit_cnt(1)='0' then
        lf_status_ena_wr_bit_cnt(2 downto 1) <= lf_status_ena_wr_bit_cnt(3 downto 2);
        lf_status_ena_wr_bit_cnt(3) <= '0';
    end if;
    
end if;
end process addr_ptr_logic_fifo;


comparators: process(lf_addr_ptr(1), lf_addr_wr_ptr(1), lf_addr_rd_ptr(1), read_pointer, write_bit_pointer, read_bit_pointer)
begin
    if unsigned(read_pointer) = unsigned(lf_addr_ptr(1)) then
        sclear_read_pointer <= '1';
    else
        sclear_read_pointer <= '0';
    end if;
    if unsigned(write_bit_pointer) = unsigned(lf_addr_wr_ptr(1)) then
        sclear_write_bit_pointer <= '1';
    else
        sclear_write_bit_pointer <= '0';
    end if;
    if unsigned(read_bit_pointer) = unsigned(lf_addr_rd_ptr(1)) then
        sclear_read_bit_pointer <= '1';
    else
        sclear_read_bit_pointer <= '0';
    end if;
end process comparators;

address_transforms: process(write_pointer,
        read_pointer,
        write_bit_pointer,
        read_bit_pointer,
        lf_addr_ptr(1),
        lf_addr_rd_ptr(1),
        wr_surv_addr_status,
        rd_surv_addr_status,
        wr_bit_addr_status,
        rd_bit_addr_status)
        
begin
    if wr_surv_addr_status='0' then
        wr_surv_addr <= write_pointer;
    else
        wr_surv_addr <= natural(V+16) - unsigned(write_pointer);
    end if;
    -- Alex 13-02-2007, I envision a problem here with the logic fifo ... 
    --  I need another logic fifo for only 3 clock delay? No, it should be a single register!
    -- or , no I should use the next logic fifo
    if rd_surv_addr_status='1' then
        rd_surv_addr <= natural(V+16) - unsigned(lf_addr_ptr(1)) + unsigned(read_pointer);
    else
        rd_surv_addr <= unsigned(lf_addr_ptr(1)) - unsigned(read_pointer);
    end if;
    if wr_bit_addr_status='0' then
        wr_bit_addr <= write_bit_pointer;
    else
        wr_bit_addr <= natural(V+16) - unsigned(write_bit_pointer);
    end if;
    if rd_bit_addr_status='1' then
        rd_bit_addr <= natural(V+16) - unsigned(lf_addr_rd_ptr(1)) + unsigned(read_bit_pointer);
    else
        rd_bit_addr <= unsigned(lf_addr_rd_ptr(1)) - unsigned(read_bit_pointer); 
    end if;
end process address_transforms;

      
    souts_ram: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => maxstates, WIDTH_ADDR => vlog_wide, NUMWORDS => 2**vlog_wide,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => survive, 
      address_a => wr_surv_addr, address_b => rd_surv_addr, 
      clock0 => clk, clock1 => clk, clocken1 => enable, 
      q_b => survivor_2_trace ); 


-- Alex, Also I need the option of loading from best state finder!
decoding: auk_vit_sel
    generic map ( inwidth => L-1, outwidth => maxstates)
    port map (selin => tr_init_state,    selout => init_trace);

fg4: for k in 1 to halfstates generate
  trace_engine_d(2*k-1) <= ((trace_engine_q(k) and not survivor_2_trace(k)) or
                                               (trace_engine_q(k+halfstates) and not survivor_2_trace(k+halfstates)));
  trace_engine_d(2*k) <= ((trace_engine_q(k) and survivor_2_trace(k)) or
                                       (trace_engine_q(k+halfstates) and survivor_2_trace(k+halfstates)));
end generate fg4;

trace_engine : Process(clk, reset)
begin
if reset='1' then
  trace_engine_q <= (others => '0');
elsif Rising_edge(clk) then
    if enable='1' then
        if load_trb_eng='1' then
            trace_engine_q <= init_trace;
        else --if enable='1' then
            trace_engine_q <= trace_engine_d;
        end if;
    end if;
end if;
end process trace_engine;

trace_decode(1) <= trace_engine_q(halfstates+1);
fg44: FOR k IN 2 TO halfstates GENERATE
  trace_decode(k) <= trace_decode(k-1) or trace_engine_q(halfstates+k);
END GENERATE fg44;
decoded_bit_to_swap(1) <= trace_decode(halfstates);

wren_bit <= not lf_status_ena_wr_bit_cnt(1);
-- Alex, 13-02-2007   I may need a wren for this ram too!

    bit_swapping: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => 1, WIDTH_ADDR => vlog_wide, NUMWORDS => 2**vlog_wide,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => wren_bit, data_a => decoded_bit_to_swap, 
      address_a => wr_bit_addr, address_b => rd_bit_addr, 
      clock0 => clk, clock1 => clk, clocken1 => enable, 
      q_b => decoded_bit_swapped ); 

-- output connections
data_available <= read_bit_to_data_val(2);
decbit <= decoded_bit_swapped(1);
sop_source <= read_push_to_source_sop(3);
eop_source <= read_pull_to_source_eop(2);


--- end new design
----------------------------

end architecture rtl_blk;    
