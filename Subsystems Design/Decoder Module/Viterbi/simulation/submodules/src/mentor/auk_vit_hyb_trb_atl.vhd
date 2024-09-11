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
-- $RCSfile: auk_vit_hyb_trb_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_trb_atl_arc_rtl.vhd,v $
--
-- $Revision: #1 $
-- Checked in by : $Author: max $
-- Last modified : $Date: 2008/07/12 $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description    :  Trace Back entity for hybrid continuous Viterbi Decoder
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
library altera_mf;
use altera_mf.altera_mf_components.all;

Entity auk_vit_hyb_trb_atl is
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
        block_delimeters : in Std_Logic;
        allow_ena_assert : out Std_Logic;
        val_source, sop_source, eop_source : out Std_Logic; -- regardless of Master or Slave mode
        dav_slave_source : out Std_Logic;
        survive : in Std_Logic_Vector(2**(L-1) downto 1);
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);
        tr_init_state : in Std_Logic_Vector(L-1 downto 1);
        sink_eop_q : in Std_Logic;
        bestadd : in Std_Logic_Vector(L-1 downto 1);
         decbit : out Std_Logic
    );    
end entity auk_vit_hyb_trb_atl;    


Architecture rtl of auk_vit_hyb_trb_atl is

Constant maxstates : NATURAL := 2**(L-1);
Constant halfstates : NATURAL := 2**(L-2);
--Constant vlog_wide : NATURAL := LOG2_ceil_table(v+2);
Constant log2_ACS_units : NATURAL := LOG2_ceil_table(ACS_units);
Constant vector_nil : Std_Logic_Vector(vlog_wide downto 1) := --natural_2_m(arg => 0, size => vlog_wide);
       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 0, SIZE => vlog_wide), SIZE => vlog_wide);
Constant vector_one : Std_Logic_Vector(vlog_wide downto 1) := --natural_2_m(arg => 1, size => vlog_wide);
       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => vlog_wide), SIZE => vlog_wide);
Constant one_maxstates : Std_Logic_Vector(maxstates downto 1) := --natural_2_m(arg => 1, size => maxstates);
       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => maxstates), SIZE => maxstates);
Constant bitsout                 : NATURAL := Calc_bitsout(L, ACS_units, v);

Constant log2_bitsout     : NATURAL := LOG2_ceil_table(bitsout+1);
--Constant bitsout_max         : NATURAL := 2**log2_bitsout;
-- SPR 191124  : no need to make bitsout power of 2 now
-- this modification below will make work even if someone enters V > 310
Constant bitsout_val : Std_Logic_Vector(log2_bitsout downto 1) := --natural_2_m(arg => bitsout, size => log2_bitsout);
       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => bitsout, SIZE => log2_bitsout), SIZE => log2_bitsout);
Constant count_limit : std_logic_vector(vlog_wide downto 1) := --natural_2_m(arg => 2**vlog_wide - 3, size => vlog_wide);
       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**vlog_wide - 3, SIZE => vlog_wide), SIZE => vlog_wide);

Subtype vector_logv is Std_Logic_Vector(vlog_wide downto 1);
type matrix_v is array(NATURAL RANGE <>) of vector_logv;
Subtype vector_l_m1 is Std_Logic_Vector(L-1 downto 1);
type matrix_l is array(NATURAL RANGE <>) of vector_l_m1;

COMPONENT auk_vit_sel
    generic ( inwidth, outwidth: NATURAL );
    port (
        selin : in Std_Logic_Vector(inwidth downto 1);
        selout : out Std_Logic_Vector(outwidth downto 1)
        );
END COMPONENT;


signal tb_length_q : Std_Logic_Vector(vlog_wide downto 1);
signal readadd, writeadd, startadd, stopadd : Std_Logic_Vector(vlog_wide downto 1);
Signal read_bestadd : std_logic_vector(vlog_wide downto 1);
Signal surv_count, bit_swap_count, symbol_count, bits_read_count, surv_proc_count : Std_Logic_Vector(vlog_wide downto 1);
signal traceff_q, traceff_d : Std_Logic_Vector(maxstates downto 1);
signal sout, statewide : Std_Logic_Vector(maxstates downto 1);
signal decout : Std_Logic_Vector(halfstates downto 1);
signal init_startadd, inc_stopadd, decbit_q : Std_Logic;
signal init_stopadd, ena_readadd, init_readadd : Std_Logic;
Signal load_trb_engine, wr_eg_start, rd_eq_stop : Std_Logic;
--signal tb_type_q : Std_Logic;
Signal bestadd_int, tr_init_state_q, bestadd_out : Std_Logic_Vector(L-1 downto 1);
signal decbit_shunt, ena_trb_pu : Std_Logic;
signal address_pointer_out : Std_Logic_Vector(vlog_wide downto 1);
signal inc_startadd : Std_Logic;
signal valid_bits_traced_orig, valid_bits_traced : Std_Logic;
signal valid_bits_traced_del : Std_Logic_Vector(2 downto 1);
signal val_source_q : Std_Logic;
signal allow_val_assert : Std_Logic;
signal sop_source_shunt, eop_source_shunt : Std_Logic;
signal eop_indicator, enough_mem : Std_Logic;

Type atl_buffer_fsm is (S0, out_idle, out_active, out_hold);

Type fsm_states is (S0, inc_start, fill, init_eop, init_rd, init_trb_engine, 
                        wait1, go_trb_engine, init_stop, inc_stop);

type read_counter_states is (idle, load, working);

Signal state_read_bits, next_state_read_bits : read_counter_states;
signal atl_buffer_state, atl_buffer_next_state : atl_buffer_fsm;
signal state_seq, next_state_seq : fsm_states;
signal out_fsm_buf : Std_Logic_Vector(2 downto 1);
signal out_fsm_seq : Std_Logic_Vector(12 downto 1);
---------
signal lf_addr_ptr : matrix_v(1 to 2);
Signal lf_pull_addr_ptr, lf_push_addr_ptr : std_logic;
signal lf_status_addr_ptr : std_logic_vector(3 downto 1);
signal lf_addr_bits_ptr : matrix_v(1 to 3);
Signal lf_pull_addr_bits_ptr, lf_push_addr_bits_ptr : std_logic;
signal lf_status_addr_bits_ptr : std_logic_vector(4 downto 1);
signal lf_eop_job_ptr : std_logic_vector(1 to 3);
Signal lf_pull_eop_job_ptr, lf_push_eop_job_ptr : std_logic;
signal lf_status_eop_job_ptr : std_logic_vector(4 downto 1);
signal lf_tr_init : matrix_l(1 to 2);
Signal lf_pull_tr_init, lf_push_tr_init : std_logic;
signal lf_status_tr_init : std_logic_vector(3 downto 1);
Signal write_bits, read_bits, stop_bits : std_logic_vector(vlog_wide downto 1);
Signal decbit_swap, decbit_int : Std_Logic_Vector(1 downto 1);
Signal rd_eq_stop_bits, ena_read_bits, load_read_bits, chunk_ready : std_logic;
Signal dav_source_int, source_ena, allow_next_sop, data_val_shunt : std_logic;
Signal dav_del : std_logic_vector(2 downto 1);
Signal data_val_pipe, source_val_int, eop_source_gen : std_logic;
signal eop_source_pipe, sop_source_pipe : Std_Logic_Vector(3 downto 1);
Signal sop_source_gen, sink_eop_del : std_logic;

begin


-- tb_length needs to be latched when the block is finished otherwise
-- errors occur!! The same that happened in parallel
 clk_tb_length: Process (clk, reset)
 begin
     if reset='1' then
         tb_length_q <= (others => '0');
         sink_eop_del <= '0';
     elsif Rising_edge(clk) then
         if state_seq=S0 or state_seq=init_stop then --or state_seq=inc_start then 
             tb_length_q <= tb_length;
         end if;
         sink_eop_del <= sink_eop_q;
     end if;
         
 end process clk_tb_length;

    

--tr_init_state_q <= tr_init_state;
-- disconnected, hence superfluous
--tb_type_q <= tb_type;


---------------------------------------------------------

-- lf_pull_addr_ptr to be controlled by main FSM
--lf_pull_addr_ptr <= inc_read_pointer;
lf_push_addr_ptr <= block_delimeters and latch_best_mark; 
-- to connect later on 
address_pointer_out <= lf_addr_ptr(1);

lf_pull_eop_job_ptr <= lf_pull_addr_bits_ptr;
--lf_push_eop_job_ptr <= '0';
lf_pull_tr_init <= load_trb_engine and eop_indicator;
lf_push_tr_init <= sink_eop_q and not sink_eop_del;

-- wr_fifo like in the parallel ought to be a delayed version of sink_eop
--wr_fifo <= (block_delimeters and latch_best_mark) or (init_startadd and inc_startadd);

addr_ptr_logic_fifo : Process (clk, reset)
begin
if reset='1' then
    lf_addr_ptr <= (others => (others => '0'));
    lf_status_addr_ptr <= "001";
    lf_addr_bits_ptr <= (others => (others => '0'));
    stop_bits <= (others => '0');
    lf_status_addr_bits_ptr <= "0001";
    lf_eop_job_ptr <= (others => '0');
    lf_status_eop_job_ptr <= "0001";
    lf_tr_init <= (others => (others => '0'));
    lf_status_tr_init <= "001";
elsif Rising_edge(clk) then
    if lf_push_addr_ptr='1' and lf_pull_addr_ptr='0' and lf_status_addr_ptr(3)='0' then 
        lf_status_addr_ptr(3 downto 2) <= lf_status_addr_ptr(2 downto 1);
        lf_status_addr_ptr(1) <= '0';
    elsif lf_push_addr_ptr='0' and lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_status_addr_ptr(2 downto 1) <= lf_status_addr_ptr(3 downto 2);
        lf_status_addr_ptr(3) <= '0';
        -- what if lf_push_addr_ptr and lf_pull_addr_ptr collide? lf_status_addr_ptr stays the same.
    end if;
    if lf_push_addr_bits_ptr='1' and lf_pull_addr_bits_ptr='0' and lf_status_addr_bits_ptr(4)='0' then 
        lf_status_addr_bits_ptr(4 downto 2) <= lf_status_addr_bits_ptr(3 downto 1);
        lf_status_addr_bits_ptr(1) <= '0';
    elsif lf_push_addr_bits_ptr='0' and lf_pull_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='0' then
        lf_status_addr_bits_ptr(3 downto 1) <= lf_status_addr_bits_ptr(4 downto 2);
        lf_status_addr_bits_ptr(4) <= '0';
    end if;
    if lf_push_eop_job_ptr='1' and lf_pull_eop_job_ptr='0' and lf_status_eop_job_ptr(4)='0' then 
        lf_status_eop_job_ptr(4 downto 2) <= lf_status_eop_job_ptr(3 downto 1);
        lf_status_eop_job_ptr(1) <= '0';
    elsif lf_push_eop_job_ptr='0' and lf_pull_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='0' then
        lf_status_eop_job_ptr(3 downto 1) <= lf_status_eop_job_ptr(4 downto 2);
        lf_status_eop_job_ptr(4) <= '0';
    end if;
    if lf_push_tr_init='1' and lf_pull_tr_init='0' and lf_status_tr_init(3)='0' then 
        lf_status_tr_init(3 downto 2) <= lf_status_tr_init(2 downto 1);
        lf_status_tr_init(1) <= '0';
    elsif lf_push_tr_init='0' and lf_pull_tr_init='1' and lf_status_tr_init(1)='0' then
        lf_status_tr_init(2 downto 1) <= lf_status_tr_init(3 downto 2);
        lf_status_tr_init(3) <= '0';
    end if;
    
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(1)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(1) <= writeadd;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(1) <= lf_addr_ptr(2);
    end if;
    if ((lf_push_addr_ptr='1' and lf_status_addr_ptr(2)='1' and lf_pull_addr_ptr='0') or
        (lf_push_addr_ptr='1' and lf_status_addr_ptr(3)='1' and lf_pull_addr_ptr='1')) then
        lf_addr_ptr(2) <= writeadd;
    elsif lf_pull_addr_ptr='1' and lf_status_addr_ptr(1)='0' then
        lf_addr_ptr(2) <= (others => '0'); 
    end if;
    if ((lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='1' and lf_pull_addr_bits_ptr='0') or
        (lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(2)='1' and lf_pull_addr_bits_ptr='1')) then
        lf_addr_bits_ptr(1) <= write_bits;
    elsif lf_pull_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='0' then
        lf_addr_bits_ptr(1) <= lf_addr_bits_ptr(2);
    end if;
    if ((lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(2)='1' and lf_pull_addr_bits_ptr='0') or
        (lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(3)='1' and lf_pull_addr_bits_ptr='1')) then
        lf_addr_bits_ptr(2) <= write_bits;
    elsif lf_pull_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='0' then
        lf_addr_bits_ptr(2) <= lf_addr_bits_ptr(3);
    end if;
    if ((lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(3)='1' and lf_pull_addr_bits_ptr='0') or
        (lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(4)='1' and lf_pull_addr_bits_ptr='1')) then
        lf_addr_bits_ptr(3) <= write_bits;
    elsif lf_pull_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='0' then
        lf_addr_bits_ptr(3) <= (others => '0'); --lf_addr_bits_ptr(4);
    end if;
    -- if ((lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(4)='1' and lf_pull_addr_bits_ptr='0') or
        -- (lf_push_addr_bits_ptr='1' and lf_status_addr_bits_ptr(5)='1' and lf_pull_addr_bits_ptr='1')) then
        -- lf_addr_bits_ptr(4) <= write_bits;
    -- elsif lf_pull_addr_bits_ptr='1' and lf_status_addr_bits_ptr(1)='0' then
        -- lf_addr_bits_ptr(4) <= (others => '0');
    -- end if;
    
    if lf_pull_addr_bits_ptr='1' then
        stop_bits <= unsigned(lf_addr_bits_ptr(1)) + natural(1);
    end if;
    
    if ((lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='1' and lf_pull_eop_job_ptr='0') or
        (lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(2)='1' and lf_pull_eop_job_ptr='1')) then
        lf_eop_job_ptr(1) <= eop_indicator;
    elsif lf_pull_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='0' then
        lf_eop_job_ptr(1) <= lf_eop_job_ptr(2);
    end if;
    if ((lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(2)='1' and lf_pull_eop_job_ptr='0') or
        (lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(3)='1' and lf_pull_eop_job_ptr='1')) then
        lf_eop_job_ptr(2) <= eop_indicator;
    elsif lf_pull_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='0' then
        lf_eop_job_ptr(2) <= lf_eop_job_ptr(3);
    end if;
    if ((lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(3)='1' and lf_pull_eop_job_ptr='0') or
        (lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(4)='1' and lf_pull_eop_job_ptr='1')) then
        lf_eop_job_ptr(3) <= eop_indicator;
    elsif lf_pull_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='0' then
        lf_eop_job_ptr(3) <= '0'; --lf_eop_job_ptr(4);
    end if;
    -- if ((lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(4)='1' and lf_pull_eop_job_ptr='0') or
        -- (lf_push_eop_job_ptr='1' and lf_status_eop_job_ptr(5)='1' and lf_pull_eop_job_ptr='1')) then
        -- lf_eop_job_ptr(4) <= eop_indicator;
    -- elsif lf_pull_eop_job_ptr='1' and lf_status_eop_job_ptr(1)='0' then
        -- lf_eop_job_ptr(4) <= '0';
    -- end if;
    
    if ((lf_push_tr_init='1' and lf_status_tr_init(1)='1' and lf_pull_tr_init='0') or
        (lf_push_tr_init='1' and lf_status_tr_init(2)='1' and lf_pull_tr_init='1')) then
        lf_tr_init(1) <= tr_init_state;
    elsif lf_pull_tr_init='1' and lf_status_tr_init(1)='0' then
        lf_tr_init(1) <= lf_tr_init(2);
    end if;
    if ((lf_push_tr_init='1' and lf_status_tr_init(2)='1' and lf_pull_tr_init='0') or
        (lf_push_tr_init='1' and lf_status_tr_init(3)='1' and lf_pull_tr_init='1')) then
        lf_tr_init(2) <= tr_init_state;
    elsif lf_pull_tr_init='1' and lf_status_tr_init(1)='0' then
        lf_tr_init(2) <= (others => '0');
    end if;
    
end if;
end process addr_ptr_logic_fifo;


--ifg1a: IF dev_family="Stratix" or dev_family="Stratix GX" or dev_family="Cyclone" GENERATE
  
    sout_ram: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => maxstates, WIDTH_ADDR => vlog_wide, NUMWORDS => 2**vlog_wide,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 0, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => survready, data_a => survive, 
      address_a => writeadd, address_b => readadd, 
      clock0 => clk, clock1 => clk,
      q_b => sout ); 
        
        
    best_state_ram: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => L-1, WIDTH_ADDR => vlog_wide, NUMWORDS => 2**vlog_wide,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 0, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => latch_best_mark, data_a => bestadd, 
      address_a => writeadd, address_b => read_bestadd, 
      clock0 => clk, clock1 => clk,
      q_b => bestadd_out ); 
        
    swap_bits: auk_vit_ram 
    GENERIC map (
      WIDTH_DATA => 1, WIDTH_ADDR => vlog_wide, NUMWORDS => 2**vlog_wide,
      USE_INIFILE=> 0, USE_CLOCKEN1=> 1, USE_ALTERA_SYNCRAM=>use_altera_syncram)
    PORT map (
      wren_a => valid_bits_traced, data_a => decbit_int, 
      address_a => write_bits, address_b => read_bits, 
      clock0 => clk, clock1 => clk, clocken1 => ena_trb_pu, 
      q_b => decbit_swap );
        
        
        
        decbit_int(1) <= decout(halfstates);
        
--end generate ifg1a;

------------------------------------
--------------------------------------


clk_FSM_atl: Process (clk, reset)
    begin
        if reset='1' then
            atl_buffer_state <= S0;
        elsif Rising_edge(clk) then
            atl_buffer_state <= atl_buffer_next_state;
        end if;
        
end process clk_FSM_atl;


source_ena <= ena_slave_source;

FSM_out : process(atl_buffer_state, dav_source_int, source_ena)

    variable atl_buffer_next_state_var : atl_buffer_fsm;

begin

  atl_buffer_next_state_var := atl_buffer_state;
  case atl_buffer_state is
    
    when S0 =>
        atl_buffer_next_state_var := out_idle;
    when out_idle =>
        if dav_source_int='1' and source_ena='1' then
            atl_buffer_next_state_var := out_active;
        elsif dav_source_int='1' and source_ena='0' then
            atl_buffer_next_state_var := out_hold;
        end if;
    when out_hold =>
        if dav_source_int='1' and source_ena='1' then 
            atl_buffer_next_state_var := out_active;
        elsif dav_source_int='0' and source_ena='1' then 
            atl_buffer_next_state_var := out_idle;
        end if;
    when out_active =>
        if source_ena='0' then 
      atl_buffer_next_state_var := out_hold;
        elsif dav_source_int='0' then
      atl_buffer_next_state_var := out_idle;
    end if;
    
    -- coverage off
    when others => atl_buffer_next_state_var := out_idle;
    -- coverage on
    end case;
    atl_buffer_next_state <= atl_buffer_next_state_var;
    
end process FSM_out;


outputs_FSM_atl: process(atl_buffer_state, dav_source_int) 

    begin
        case atl_buffer_state is
        
        when S0 =>
            allow_val_assert <= '0';
            ena_trb_pu <= '0';
        when out_idle =>
            allow_val_assert <= dav_source_int;
            ena_trb_pu <= '1';
        when out_active =>
            allow_val_assert <= dav_source_int;
            ena_trb_pu <= '1';
        when out_hold =>
            allow_val_assert <= '1';
            ena_trb_pu <= '0';
        -- coverage off
        when others => 
            allow_val_assert <= '0';
            ena_trb_pu <= '0';
        -- coverage on
        end case;
        
end process outputs_FSM_atl;

-------------------------------
-- New re-development of TRB unit with FIFO for end state address
-- 

read_bestadd_proc: process(init_readadd, lf_status_addr_ptr(1), startadd, address_pointer_out)
begin
    if init_readadd='1' and lf_status_addr_ptr(1)='1' then
        read_bestadd <= startadd - natural(1);
    elsif init_readadd='1' and lf_status_addr_ptr(1)='0' then
        read_bestadd <= address_pointer_out;
    else  
        read_bestadd <= (others => '0');
    end if;
end process read_bestadd_proc;


start_stop_proc : Process (clk, reset)

    variable eop_indicator_var : Std_Logic;
    variable tmp : unsigned(vlog_wide downto 1);

begin
if reset = '1' then
    startadd <= (others => '0');
    stopadd  <= (others => '0');
    writeadd <= (others => '0');
    readadd <= (others => '0');
    --symbol_count <= (others => '0');
    bit_swap_count <= (others => '1');
    surv_count <= (others => '0');
    bits_read_count <= (others => '0'); 
    surv_proc_count <= (others => '0');
    write_bits <= (others => '0');
    read_bits <= (others => '0');
    eop_indicator <= '0';
elsif Rising_edge(clk) then
    if latch_best_mark='1' then
        writeadd <= writeadd + natural(1); -- Up
    end if;
    if init_startadd='1' and lf_status_addr_ptr(1)='0' then
    startadd <= address_pointer_out;
    elsif inc_startadd='1' then
        startadd <= unsigned(stopadd) + unsigned(tb_length_q) + unsigned(bitsout_val);
    end if;
    if init_stopadd='1' then 
        stopadd <= unsigned(address_pointer_out) + natural(1);
    elsif inc_stopadd='1' then
        stopadd <= unsigned(stopadd) + unsigned(bitsout_val);
    end if;
    
    --if survready='1' and source_val_int='0' then
       --symbol_count <= unsigned(symbol_count) + natural(1);
    --elsif survready='0' and source_val_int='1' then
       --symbol_count <= unsigned(symbol_count) - natural(1);
  --end if;
    -- if survready='1' and valid_bits_traced='0' then
       -- surv_count <= unsigned(surv_count) + natural(1);
    -- elsif survready='0' and valid_bits_traced='1' then
       -- surv_count <= unsigned(surv_count) - natural(1);
  -- end if;
    if survready='1' and (valid_bits_traced_orig='0' or rd_eq_stop='0') then
       surv_count <= unsigned(surv_count) + natural(1);
    elsif survready='1' and valid_bits_traced_orig='1' and rd_eq_stop='1' then
       surv_count <= unsigned(surv_count) - unsigned(surv_proc_count);
    elsif survready='0' and valid_bits_traced_orig='1' and rd_eq_stop='1' then
        tmp := unsigned(surv_count) - unsigned(surv_proc_count);
       --surv_count <= unsigned(surv_count) - unsigned(surv_proc_count) - natural(1);
        surv_count <= tmp - natural(1);
  end if;
    -- if valid_bits_traced='1' and source_val_int='0' then
       -- bit_swap_count <= unsigned(bit_swap_count) + natural(1);
    -- elsif valid_bits_traced='0' and source_val_int='1' then
       -- bit_swap_count <= unsigned(bit_swap_count) - natural(1);
  -- end if;
    if valid_bits_traced='1' and (rd_eq_stop_bits='0' or ena_trb_pu='0' or ena_read_bits='0') then
       bit_swap_count <= unsigned(bit_swap_count) - natural(1);
    elsif valid_bits_traced='1' and rd_eq_stop_bits='1' and ena_trb_pu='1' and ena_read_bits='1' then
       bit_swap_count <= unsigned(bit_swap_count) + unsigned(bits_read_count);
    elsif valid_bits_traced='0' and rd_eq_stop_bits='1' and ena_trb_pu='1' and ena_read_bits='1' then
      tmp := unsigned(bit_swap_count) + unsigned(bits_read_count);
       --bit_swap_count <= unsigned(bit_swap_count) - unsigned(bits_read_count) - natural(1);
        bit_swap_count <= tmp + natural(1);
  end if;
    if rd_eq_stop='1' then
       surv_proc_count <= (others => '0');
    elsif valid_bits_traced_orig='1' then
       surv_proc_count <= unsigned(surv_proc_count) + natural(1);
  end if;
    if rd_eq_stop_bits='1' and ena_trb_pu='1' then
       bits_read_count <= (others => '0');
    elsif ena_read_bits='1' and ena_trb_pu='1' then
       bits_read_count <= unsigned(bits_read_count) + natural(1);
  end if;


    if init_readadd='1' and lf_status_addr_ptr(1)='1' then
        readadd <= startadd - natural(1);
  elsif init_readadd='1' and lf_status_addr_ptr(1)='0' then
    readadd <= address_pointer_out;
    elsif rd_eq_stop='0' and ena_readadd='1' then 
        readadd <= unsigned(readadd) - natural(1);
    end if;
  if valid_bits_traced='1' then
    write_bits <= unsigned(write_bits) + natural(1);
  end if;
    -- this read counter to be controlled by FSM 
    if load_read_bits='1' then
        read_bits <= lf_addr_bits_ptr(1); 
    elsif ena_read_bits='1' and ena_trb_pu='1' then 
        read_bits <= unsigned(read_bits) - natural(1);
    end if;
    
    eop_indicator_var := eop_indicator;
    if state_seq=init_eop then
        eop_indicator_var := '1';
    elsif rd_eq_stop='1' then
        eop_indicator_var := '0';
    end if;
    eop_indicator <= eop_indicator_var;

end if;
end process start_stop_proc;


eq_comp5: process(writeadd, startadd, stopadd)

    variable diff_1, diff_2 : Std_Logic_Vector(vlog_wide downto 1);

begin
    diff_1 := unsigned(writeadd) - unsigned(stopadd);
    diff_2 := unsigned(startadd) - unsigned(stopadd);
    if unsigned(diff_1) >= unsigned(diff_2) then
        wr_eg_start <= '1';
    else
        wr_eg_start <= '0';
    end if;
    -- if unsigned(writeadd) >= unsigned(startadd) then
        -- wr_eg_start <= '1';
    -- else
        -- wr_eg_start <= '0';
    -- end if;
end process eq_comp5;

eq_comp6: process(readadd, stopadd, eop_indicator, ena_readadd) 
begin
    if unsigned(readadd) = unsigned(stopadd) then
        rd_eq_stop <= '1';
    else
        rd_eq_stop <= '0';
    end if;
    if eop_indicator='1' then
            valid_bits_traced_orig <= ena_readadd;
  else
        if (unsigned(readadd) - unsigned(stopadd)) < unsigned(bitsout_val) then
        valid_bits_traced_orig <= ena_readadd; --'1';
    else
            valid_bits_traced_orig <= '0';
    end if;
  end if;
    
end process eq_comp6;


neq_comp7: process(lf_status_addr_ptr(1), stopadd, tb_length_q, address_pointer_out, bit_swap_count)
    variable tmp : std_logic_vector(vlog_wide downto 1); 
begin
    
    if lf_status_addr_ptr(1)='0' then
    tmp := unsigned(address_pointer_out) - unsigned(stopadd);
    else
        tmp := unsigned(tb_length_q) + unsigned(bitsout_val);
    end if;
    if unsigned(tmp) < unsigned(bit_swap_count) then
        enough_mem <= '1';
    else
        enough_mem <= '0';
    end if;
end process neq_comp7;


eq_comp10: process(read_bits, stop_bits)

begin
    if unsigned(read_bits) = unsigned(stop_bits) then
        rd_eq_stop_bits <= '1';
    else
        rd_eq_stop_bits <= '0';
    end if;
end process eq_comp10;


-- REMINDER: If logic FIFOs are re-sized then in here the coefficients have to be changed in signals
--  lf_status_...
outputs_FSM_ena_assert_ctrl: process(lf_status_addr_ptr(3), surv_count, lf_status_addr_bits_ptr(4),  
                                                                         lf_status_eop_job_ptr(4), lf_status_tr_init(3)) 

    begin
        if (unsigned(surv_count) > unsigned(count_limit)) then 
             allow_ena_assert <= '0';
        -- REMINDER: If logic FIFOs are re-sized then in here the coefficients have to be changed
        elsif lf_status_addr_ptr(3)='1' or    lf_status_addr_bits_ptr(4)='1' or    lf_status_eop_job_ptr(4)='1' or
              lf_status_tr_init(3)='1' then 
            allow_ena_assert <= '0';
        else
            allow_ena_assert <= '1';
        end if;
        
end process outputs_FSM_ena_assert_ctrl;


del_valid_bits: Process (clk, reset)
    begin
        if reset='1' then
            valid_bits_traced_del <= (others => '0');
        elsif Rising_edge(clk) then
            valid_bits_traced_del(1) <= valid_bits_traced_orig;
            valid_bits_traced_del(2) <= valid_bits_traced_del(1);
        end if;
        
end process del_valid_bits;




FSM_new: process(state_seq, wr_eg_start, rd_eq_stop, lf_status_addr_ptr(1), 
                 sink_sop_q, eop_indicator, lf_status_addr_bits_ptr(4), enough_mem)

  variable next_state_seq_var : fsm_states;

    begin
        next_state_seq_var := state_seq;
        case state_seq is
        when S0 =>
            if sink_sop_q='1' then
                next_state_seq_var := inc_start;
            else
                next_state_seq_var := S0;
            end if;
        when inc_start =>
            next_state_seq_var := fill;
        when fill =>
            if lf_status_addr_ptr(1)='0' and lf_status_addr_bits_ptr(4)='0' and enough_mem='1' then
          next_state_seq_var := init_eop; --wait0;
      elsif wr_eg_start='1' and lf_status_addr_bits_ptr(4)='0' and enough_mem='1' then
-- looks like some wait states are needed here before loading the best state 
-- from memory
                next_state_seq_var := init_rd;
            else
                next_state_seq_var := fill;
            end if;
    when init_eop => 
      next_state_seq_var := wait1; --init_rd;
        when init_rd =>
      next_state_seq_var := wait1;
        when wait1 => 
            next_state_seq_var := init_trb_engine;
    when init_trb_engine =>
            next_state_seq_var := go_trb_engine;
        when go_trb_engine =>
            if rd_eq_stop='1' and eop_indicator='1' then
                next_state_seq_var := init_stop; 
            elsif rd_eq_stop='1' and eop_indicator='0' then
                next_state_seq_var := inc_stop; 
            else
                next_state_seq_var := go_trb_engine;
            end if;
        when init_stop =>
            next_state_seq_var := inc_start;
        when inc_stop =>
            next_state_seq_var := inc_start;

        -- coverage off
        when others => next_state_seq_var := S0;
        -- coverage on
        end case;
        next_state_seq <= next_state_seq_var;
        
    end process FSM_new;


clk_FSM_new: Process (clk, reset)
    begin
        if reset='1' then
            state_seq <= S0;
            state_read_bits <= idle;
        elsif Rising_edge(clk) then
      state_seq <= next_state_seq;
            state_read_bits <= next_state_read_bits;
        end if;
end process clk_FSM_new;


init_startadd <= out_fsm_seq(1);
inc_startadd <= out_fsm_seq(2);
init_stopadd <= out_fsm_seq(3);
inc_stopadd <= out_fsm_seq(4);
ena_readadd <= out_fsm_seq(5);
init_readadd <= out_fsm_seq(6);
load_trb_engine <= out_fsm_seq(7);
--ena_trb_engine <= out_fsm_seq(8);
lf_pull_addr_ptr <= out_fsm_seq(9);
-- I don't like eop_activate
--eop_activate <= out_fsm_seq(10);
--ena_shift_lfsr <= out_fsm_seq(11);
--valid_bits_trb_state <= out_fsm_seq(12);

valid_bits_traced <= valid_bits_traced_del(2);


outputs_FSM_new: process(state_seq, rd_eq_stop)

    begin
        case state_seq is
        when S0 =>
            out_fsm_seq <= "000000000010";
        when inc_start =>
            out_fsm_seq <= "001000000010";
        when fill =>
            out_fsm_seq <= "000000000000";
    when init_eop =>
      out_fsm_seq <= "000000100001";
        when init_rd => 
      out_fsm_seq <= "000000100000";
    when wait1 =>
            out_fsm_seq <= "000000010000";
    when init_trb_engine =>
            out_fsm_seq <= "000001010000";
        when go_trb_engine =>
            out_fsm_seq <= "100010010000";
        when inc_stop => 
            out_fsm_seq <= "000000001000";
        when init_stop => 
            out_fsm_seq <= "000100000100";

        -- coverage off
        when others => 
            out_fsm_seq <= "000000000000";
        -- coverage on
        end case;
        if state_seq=go_trb_engine and rd_eq_stop='1' then
            lf_push_eop_job_ptr <= '1';
        else
            lf_push_eop_job_ptr <= '0';
        end if;
        
end process outputs_FSM_new;

chunk_ready <= valid_bits_traced_del(2) and not valid_bits_traced_del(1);  
lf_push_addr_bits_ptr <= chunk_ready; 

FSM_read_bits: process(state_read_bits, chunk_ready,  
                                                        rd_eq_stop_bits, lf_status_addr_bits_ptr(2 downto 1), ena_trb_pu)


begin
    case state_read_bits is

    when idle => 
    -- I have to load if chunk_top_ready or if there is data available in the FIFO
        if chunk_ready='1' then
        next_state_read_bits <= load;
        else
            next_state_read_bits <= idle;
        end if;
    -- here I make the decission to go forward if it in the right order
    when load => 
       next_state_read_bits <= working;
    when working => 
        if (chunk_ready='1' or lf_status_addr_bits_ptr(2 downto 1)="00") and rd_eq_stop_bits='1' and ena_trb_pu='1' then
            next_state_read_bits <= load;
        elsif chunk_ready='0' and lf_status_addr_bits_ptr(2 downto 1)="10" and rd_eq_stop_bits='1' and ena_trb_pu='1' then
            next_state_read_bits <= idle;
        else
            next_state_read_bits <= working;
        end if;
    -- coverage off
    when others => next_state_read_bits <= idle;
    -- coverage on
    end case;
    
end process FSM_read_bits;


outputs_FSM_read_bits: process(state_read_bits, rd_eq_stop_bits, ena_trb_pu)

    begin
        case state_read_bits is
        when idle =>
            ena_read_bits <= '0';
            load_read_bits <= '0';
            lf_pull_addr_bits_ptr <= '0';
        when load =>
            ena_read_bits <= '0';
            load_read_bits <= '1';
            lf_pull_addr_bits_ptr <= '0';
        when working =>
            ena_read_bits <= '1';
            load_read_bits <= '0';
            if rd_eq_stop_bits='1' and ena_trb_pu='1' then
                lf_pull_addr_bits_ptr <= '1';
            else
                lf_pull_addr_bits_ptr <= '0';
            end if;
        -- coverage off
        when others => 
            ena_read_bits <= '0';
            load_read_bits <= '0';
            lf_pull_addr_bits_ptr <= '0';
        -- coverage on
        end case;
        
end process outputs_FSM_read_bits;

-- to sort out SPR 195369
bestadd_int <= bestadd_out;

-- muxbest: process(lf_tr_init(1), bestadd_out, lf_status_addr_ptr(1)) 
-- 
-- begin
    -- if lf_status_addr_ptr(1)='0' then
      -- bestadd_int <= lf_tr_init(1);
    -- else
      -- bestadd_int <= bestadd_out;
    -- end if;
-- end process muxbest;

decoding: auk_vit_sel
    generic map ( inwidth => L-1, outwidth => maxstates)
    port map (selin => bestadd_int,    selout => statewide);

fg2: FOR k IN 1 TO halfstates GENERATE
    traceff_d(2*k-1) <= ((traceff_q(k) and not sout(k)) or (traceff_q(k+halfstates) and not sout(k+halfstates)));
  traceff_d(2*k) <= ((traceff_q(k) and sout(k)) or (traceff_q(k+halfstates) and sout(k+halfstates)));
END GENERATE fg2; 

traceff : Process (clk, reset)
begin
if reset = '1' then
    traceff_q <= one_maxstates;
elsif Rising_edge(clk) then
    if load_trb_engine='1' then
        traceff_q <= statewide;
    else 
        traceff_q <= traceff_d;
    end if;
end if;
end process traceff;

decode: process(traceff_q)
  variable decout_var : Std_Logic_Vector(halfstates downto 1);
    
    Constant chunk_states : NATURAL := 2**3;  -- for L=5
begin
    decout_var(1) := traceff_q(halfstates+1);
  fg3: FOR k IN 2 TO halfstates loop
    decout_var(k) := decout_var(k-1) or traceff_q(halfstates+k);
  end loop fg3;
    decout <= decout_var;
  
end process decode;

gen_eop_source: process(rd_eq_stop_bits, lf_eop_job_ptr(1), state_read_bits)

begin
    if state_read_bits=working then
        eop_source_gen <= rd_eq_stop_bits and lf_eop_job_ptr(1);
    else
        eop_source_gen <= '0';
    end if;
end process gen_eop_source;



dec_bit : Process (clk, reset)

  variable bit_out_lfsr, decbit_d, dav_lfsr, sop_source_d, eop_source_d, dav_source_d : Std_Logic;
    variable data_val_d : Std_Logic;
  
begin
if reset = '1' then
    decbit_q <= '0';
  decbit_shunt <= '0';
    val_source_q <= '0';
    sop_source_shunt <= '0';
  eop_source_shunt <= '0';
    -----------
    dav_del <= (others => '0');
    data_val_shunt <= '0';
    allow_next_sop <= '1';
    data_val_pipe <= '0';
    eop_source_pipe <= (others => '0');
    --sop_source_pipe <= (others => '0');
    sop_source_pipe(1) <= '1';
    sop_source_pipe(2) <= '0';
    sop_source_pipe(3) <= '0';
    
    
elsif Rising_edge(clk) then
    if ena_trb_pu='1' then
        decbit_d := decbit_swap(1) and dav_source_int;  
        data_val_d := dav_source_int; 
        eop_source_d := eop_source_pipe(2);
        sop_source_d := sop_source_pipe(2) and allow_next_sop; --sop_source_gen; 
    else
        decbit_d := decbit_shunt;
        data_val_d := data_val_shunt;
        eop_source_d := eop_source_shunt;
        sop_source_d := sop_source_shunt;
    end if;
  if ena_trb_pu='1' then
        dav_del(1) <= ena_read_bits;
        dav_del(2) <= dav_del(1);

        eop_source_pipe(1) <= eop_source_gen; 
        --sop_source_pipe(1) <= sop_source_gen and allow_next_sop; --allow_next_sop; -- to be edited
        data_val_shunt <= dav_source_int; 
        eop_source_pipe(2) <= eop_source_pipe(1);
        --sop_source_pipe(2) <= sop_source_pipe(1) and allow_next_sop;

        decbit_shunt <= decbit_swap(1) and dav_source_int; 
        eop_source_shunt <= eop_source_pipe(2);
        sop_source_shunt <= sop_source_pipe(2) and allow_next_sop;  --sop_source_gen;
  end if;
    if ena_trb_pu='1' and sop_source_pipe(1)='0' then
        sop_source_pipe(1) <= dav_source_int and allow_next_sop; 
    elsif data_val_d='1' and ena_slave_source='1' and sop_source_d='1' and sop_source_pipe(1)='1' then
        sop_source_pipe(1) <= '0';
    end if;
    if ena_trb_pu='1' and sop_source_pipe(2)='0' then
        sop_source_pipe(2) <= sop_source_pipe(1) and allow_next_sop;
    elsif data_val_d='1' and ena_slave_source='1' and sop_source_d='1' and sop_source_pipe(2)='1' then
        sop_source_pipe(2) <= '0';
    end if;
    if ena_slave_source='1' then
        decbit_q <= decbit_d;
        data_val_pipe <= data_val_d;
        eop_source_pipe(3) <= eop_source_d;
        --sop_source_pipe(3) <= sop_source_d and allow_next_sop;
    end if;
    if data_val_d='1' and ena_slave_source='1' then
        sop_source_pipe(3) <= sop_source_d;
    end if;

  val_source_q <= ena_slave_source and allow_val_assert;

    if ena_trb_pu='1' and eop_source_gen='1' and allow_next_sop='0' then
    --if eop_source_pipe(3)='1' then
        allow_next_sop <= '1';
    --elsif sop_source_gen='1' then
    --elsif sop_source_pipe(3)='1' and data_val_pipe='1' then
    elsif data_val_d='1' and ena_slave_source='1' and sop_source_d='1' then
        allow_next_sop <= '0';
    end if;
        
end if;
end process dec_bit;

dav_source_int <= dav_del(2);
--sop_source_gen <= dav_source_int and allow_next_sop;

decbit <= decbit_q;
dav_slave_source <= '1'; --dav_source_q;
val_source <= source_val_int; 
source_val_int <= val_source_q and data_val_pipe;
eop_source <= eop_source_pipe(3);
sop_source <= sop_source_pipe(3) and data_val_pipe;

end architecture rtl;

