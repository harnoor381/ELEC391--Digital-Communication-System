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
-- $RCSfile: auk_vit_par_ber_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/Parallel/atlantic/auk_vit_par_ber_atl_arc_rtl.vhd,v $
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

Entity auk_vit_par_ber_atl is
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
--                node_sync     : STRING := "used";
                ga : STRING := "91 91";
                gb : STRING := "121 101";
                gc : STRING := "0 125";
                gd : STRING := "0 0";
                ge : STRING := "0 0";
                gf : STRING := "0 0";
                gg : STRING := "0 0";
                opt_par : STRING := "None";  -- none, continuous or block
                dev_family : STRING := "Stratix"
    );
    Port (
        clk : in std_logic; 
        reset : in Std_Logic;
        ber_clear : in std_logic;
        sink_val, outvalid, sop_source, eop_source, sink_sop, sink_eop : in Std_Logic;
        data_ram_in : in Std_Logic_Vector(2*n_max downto 1);
        sel_code : in Std_Logic_Vector(sel_code_size downto 1);
        decbit : in Std_Logic;
         numerr : out Std_Logic_Vector(numerr_size downto 1)
    );    
end entity auk_vit_par_ber_atl;    


Architecture rtl of auk_vit_par_ber_atl is


Constant GND : Std_Logic := '0';
Constant VCC : Std_Logic := '1';
Constant log2_4v : NATURAL := vlog_wide+2;

Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant L_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);
Constant binary_table : Vector_2D(0 to ncodes-1) := Build_binary_table(ncodes);

Constant log2_n_max : NATURAL := LOG2_ceil_Table(n_max+1);
--Constant num_bit_err_matrix : pla_table_t(2 downto 1) :=
                 --std_logic_matrix(two_pow_bitsout downto 1, bitsout+log2_bitsout downto 1) := 
--                 gen_num_bit_errors(m => 1); --bitsout);

COMPONENT auk_vit_add_tre
    Generic (
                n : NATURAL := 2
    );
        Port (
              diffs : in Std_Logic_Vector(n downto 1);
              errs : out Std_Logic_Vector(LOG2_ceil_table(n+1) downto 1)
    );
END COMPONENT;


COMPONENT auk_vit_var_enc
    Generic (
        n : NATURAL := 2;
        L_max : NATURAL := 7;
        L_code    : NATURAL  := 7;
        pol_sel : NATURAL := 1; 
        ga : STRING := "91_91";
        gb : STRING := "121_101";
        gc : STRING := "0_125";
        gd : STRING := "0_0";
        ge : STRING := "0_0";
        gf : STRING := "0_0";
        gg : STRING := "0_0"
    );
    Port (
        state : in Std_Logic_Vector(L_max downto 1);
         vector : out Std_Logic_Vector(n downto 1)
    );    
END COMPONENT;


Subtype Vector_2D_bit is Vector_2D(1 downto 1);
Type Vector_3D is ARRAY(NATURAL RANGE<>) of Vector_2D_bit;
Subtype vector_sc  is Std_Logic_Vector(sel_code_size downto 1);
type matrix_sc is array(NATURAL RANGE <>) of vector_sc;


signal readadd, writeadd : Std_Logic_Vector(log2_4v downto 1);
signal err_accum_ber : Std_Logic_Vector(numerr_size downto 1);
signal sel_code_q, sel_code_pre_q : Std_Logic_Vector(sel_code_size downto 1);
signal err_add : Std_Logic_Vector(log2_n_max downto 1);
signal sout : Std_Logic_Vector(2*n_max downto 1);
signal hypo : Vector_2D(1 to ncodes);

signal err, hypodel_q : Std_Logic_Vector(n_max downto 1);
signal shift_d, shift_q : Std_Logic_Vector(L_max downto 1);
signal val_filter, decbit_del_q : Std_Logic;
Signal source_val_del, sop_source_del, sink_val_del, eop_source_del : Std_Logic_Vector(3 downto 1);

signal lf_sc_ptr : matrix_sc(1 to 4);
Signal lf_pull_sc_ptr, lf_push_sc_ptr : std_logic;
signal lf_status_sc_ptr : std_logic_vector(5 downto 1);
signal ber_clear_q : std_logic;
--signal ber_sclear_one : std_logic;
--signal ber_sclear_two : std_logic;


begin


ifg_not_cont3: if opt_par/="Continuous" generate

sink_val_clk : Process (clk, reset)
begin
if reset = '1' then
    --sink_val_q <= '0';
    sop_source_del <= (others => '0');
    eop_source_del <= (others => '0');
    sel_code_pre_q <= (others => '0');
    source_val_del <= (others => '0');
    --sel_code_q <= (others => '0');
    sink_val_del <= (others => '0');
    val_filter <= '0';
    ber_clear_q <= '0';
elsif Rising_edge(clk) then
    ber_clear_q <= ber_clear;
    -- Alex 02-02-2007  this is all for standard non-optimized decoder. Continuous optimization
    --  doesn't have, nor need sink_sop
    if sink_val='1' and sink_sop='1' then
        val_filter <= '1';
    elsif sink_val='1' and sink_eop='1' and sink_sop='0' then
        val_filter <= '0';
    end if;
    if sink_sop='1' and sink_val='1' then
        sel_code_pre_q <= sel_code;
    end if;
    if sop_source='1' then
        --sel_code_q <= sel_code_pre_q;
    end if;
    sink_val_del(1) <= sink_val and (val_filter or sink_sop);
    sink_val_del(3 downto 2) <= sink_val_del(2 downto 1);
    source_val_del(1) <= outvalid;
    source_val_del(2) <= source_val_del(1);
    source_val_del(3) <= source_val_del(2);
    sop_source_del(1) <= sop_source;
    sop_source_del(2) <= sop_source_del(1);
    sop_source_del(3) <= sop_source_del(2);
    eop_source_del(1) <= eop_source;
    eop_source_del(2) <= eop_source_del(1);
    eop_source_del(3) <= eop_source_del(2);
end if;
end process sink_val_clk;

end generate ifg_not_cont3;


ifg_cont3: if opt_par="Continuous" generate

sink_val_clk : Process (clk, reset)
begin
if reset = '1' then
    --sink_val_q <= '0';
    sop_source_del <= (others => '0');
    eop_source_del <= (others => '0');
    sel_code_pre_q <= (others => '0');
    source_val_del <= (others => '0');
    --sel_code_q <= (others => '0');
    sink_val_del <= (others => '0');
    --val_filter <= '0';
    ber_clear_q <= '0';
elsif Rising_edge(clk) then
    ber_clear_q <= ber_clear;
    -- if sink_val='1' and sink_sop='1' then
        -- val_filter <= '1';
    -- elsif sink_val='1' and sink_eop='1' and sink_sop='0' then
        -- val_filter <= '0';
    -- end if;
    if sink_val='1' then
        sel_code_pre_q <= sel_code;
    end if;
    
    sink_val_del(1) <= sink_val; -- and (val_filter or sink_sop);
    sink_val_del(3 downto 2) <= sink_val_del(2 downto 1);
    source_val_del(1) <= outvalid;
    source_val_del(2) <= source_val_del(1);
    source_val_del(3) <= source_val_del(2);
    sop_source_del(1) <= sop_source;
    sop_source_del(2) <= sop_source_del(1);
    sop_source_del(3) <= sop_source_del(2);
    eop_source_del(1) <= eop_source;
    eop_source_del(2) <= eop_source_del(1);
    eop_source_del(3) <= eop_source_del(2);
end if;
end process sink_val_clk;

end generate ifg_cont3;

readadd_atl : Process (clk, reset)
begin
if reset = '1' then
    readadd <= (others => '0');
elsif Rising_edge(clk) then
    if outvalid='1' then
        readadd <= readadd + natural(1);
    end if;
end if;
end process readadd_atl;

-- if moving data_ram_in process to _bmp then I have to connect this load to a 
--  different signal coming from bmp to store data_ram_in from rrffa.

writeadd_atl : Process (clk, reset)
begin
if reset = '1' then
    writeadd <= (others => '0');
elsif Rising_edge(clk) then
    if sink_val_del(2)='1' then
        writeadd <= writeadd + natural(1);
    end if;
end if;
end process writeadd_atl;


    store: altsyncram 
   GENERIC map (
      operation_mode => "DUAL_PORT", width_a => 2*n_max, widthad_a => log2_4v, numwords_a => 2**log2_4v,
      outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
      outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
      wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, address_reg_b => "CLOCK0",
            width_b => 2*n_max, widthad_b => log2_4v, numwords_b => 2**log2_4v,
            rdcontrol_reg_b => "UNUSED",
            outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
            indata_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
            indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
      read_during_write_mode_mixed_ports => "DONT_CARE", ram_block_type => "AUTO",
      intended_device_family => dev_family)
   PORT map (
      wren_a => sink_val_del(2), data_a => data_ram_in, address_a => writeadd,
            address_b => readadd, clock0 => clk, 
      q_b => sout );



-- use hypodel_q instead of hypo because of muxing
fg7: for k in 1 to n_max generate
  err(k) <= (sout(k) xor hypodel_q(k)) and sout(n_max+k);
end generate fg7;

calc_errors: auk_vit_add_tre
    Generic map (n => n_max)
    Port map (diffs => err,    errs => err_add );


-- To be moved to inside a process and shift_d converted into a variable

-- Alex 25-01-2007  for parallel continuous sop_source_del doesn't exist!!
ifg_not_cont2: if opt_par/="Continuous" generate 
    shift_d(L_max) <= decbit_del_q;
    shift_d(L_max-1 downto 1) <= shift_q(L_max downto 2) and (L_max downto 2 => not sop_source_del(1));
end generate ifg_not_cont2;

ifg_cont2: if opt_par="Continuous" generate 
    shift_d(L_max) <= decbit_del_q;
    shift_d(L_max-1 downto 1) <= shift_q(L_max downto 2); -- and (L_max downto 2 => not sop_source_del(1));
end generate ifg_cont2;

shift : Process (clk, reset)
begin
if reset = '1' then
    shift_q <= (others => '0');
    decbit_del_q <= '0';
elsif Rising_edge(clk) then
    if outvalid='1' then
        decbit_del_q <= decbit;
    end if;
    -- if ber_clear_q='1' then
        -- shift_q <= (others => '0');
    -- els
    if source_val_del(1)='1' then
        shift_q <= shift_d;
    end if;
end if;
end process shift;
                    
fg4: For I in 1 to ncodes generate

  encoding: auk_vit_var_enc
      Generic map (n => n_list(I), L_max => L_max, L_code => L_list(I), pol_sel => I, 
                                    ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)
      Port map (state => shift_q(L_max downto 1),
                           vector => hypo(I)(n_list(I) downto 1) );

end generate fg4;


-- I think I can remove the FF at the end of the MUX 

ifg_cont: if opt_par="Continuous" and (ncodes > 1) generate
    
    selcode_reg: Process (clk, reset)
    begin
    if reset='1' then
        sel_code_q <= (others => '0');
    elsif Rising_edge(clk) then
        sel_code_q <= sel_code;
    end if;
end process selcode_reg;

end generate ifg_cont;

ifg_not_cont: if (opt_par/="Continuous")  and (ncodes > 1) generate

lf_push_sc_ptr <= sink_val and sink_sop;
lf_pull_sc_ptr <= eop_source_del(3) and source_val_del(3);

    selcode_logic_fifo : Process (clk, reset)
    begin
    if reset='1' then
        lf_sc_ptr <= (others => (others => '0'));
        lf_status_sc_ptr <= "00001";
    elsif Rising_edge(clk) then
        if lf_push_sc_ptr='1' and lf_pull_sc_ptr='0' and lf_status_sc_ptr(5)='0' then 
            lf_status_sc_ptr(5 downto 2) <= lf_status_sc_ptr(4 downto 1);
            lf_status_sc_ptr(1) <= '0';
        elsif lf_push_sc_ptr='0' and lf_pull_sc_ptr='1' and lf_status_sc_ptr(1)='0' then
            lf_status_sc_ptr(4 downto 1) <= lf_status_sc_ptr(5 downto 2);
            lf_status_sc_ptr(5) <= '0';
            -- what if lf_push_sc_ptr and lf_pull_sc_ptr collide? lf_status_sc_ptr stays the same.
        end if;
        
        if ((lf_push_sc_ptr='1' and lf_status_sc_ptr(1)='1' and lf_pull_sc_ptr='0') or
                (lf_push_sc_ptr='1' and lf_status_sc_ptr(2)='1' and lf_pull_sc_ptr='1')) then
            lf_sc_ptr(1) <= sel_code;
        elsif lf_pull_sc_ptr='1' and lf_status_sc_ptr(1)='0' then
            lf_sc_ptr(1) <= lf_sc_ptr(2);
        end if;
        if ((lf_push_sc_ptr='1' and lf_status_sc_ptr(2)='1' and lf_pull_sc_ptr='0') or
                (lf_push_sc_ptr='1' and lf_status_sc_ptr(3)='1' and lf_pull_sc_ptr='1')) then
            lf_sc_ptr(2) <= sel_code;
        elsif lf_pull_sc_ptr='1' and lf_status_sc_ptr(1)='0' then
            lf_sc_ptr(2) <= lf_sc_ptr(3);
        end if;
        if ((lf_push_sc_ptr='1' and lf_status_sc_ptr(3)='1' and lf_pull_sc_ptr='0') or
            (lf_push_sc_ptr='1' and lf_status_sc_ptr(4)='1' and lf_pull_sc_ptr='1')) then
            lf_sc_ptr(3) <= sel_code;
        elsif lf_pull_sc_ptr='1' and lf_status_sc_ptr(1)='0' then
            lf_sc_ptr(3) <= lf_sc_ptr(4);
        end if;
        if ((lf_push_sc_ptr='1' and lf_status_sc_ptr(4)='1' and lf_pull_sc_ptr='0') or
            (lf_push_sc_ptr='1' and lf_status_sc_ptr(5)='1' and lf_pull_sc_ptr='1')) then
            lf_sc_ptr(4) <= sel_code;
        elsif lf_pull_sc_ptr='1' and lf_status_sc_ptr(1)='0' then
            lf_sc_ptr(4) <= (others => '0');
        end if;
        
    end if;
    end process selcode_logic_fifo;

    sel_code_q <= lf_sc_ptr(1);

end generate ifg_not_cont;

if_mux : if ncodes > 1 generate
    hypo_mux : Process(hypo, sel_code_q)

    variable hypo_mux_p :    Vector_2D(0 to ncodes);
        variable hypodel_d, hypo_tmp : Std_Logic_Vector(n_max downto 1);
    variable tmp_and_sel : Std_Logic_Vector(sel_code_q'HIGH downto 0);

  begin
    tmp_and_sel(0) := '1';
    hypo_mux_p(0)(n_max downto 1) := (others => '0');
    or_loop: For I in 1 to ncodes loop
        and_loop: For J in 1 to sel_code_q'HIGH loop
            if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
                tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_q(J);
            else  -- bit J of I-1 is 1
                tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_q(J);
            end if;
        end loop and_loop;
            hypo_tmp(n_list(I) downto 1) := hypo(I)(n_list(I) downto 1);
            if n_max > n_list(I) then
                hypo_tmp(n_max downto n_list(I)+1) := (others => '0');
            end if;
        hypo_mux_p(I)(n_max downto 1) :=
        hypo_mux_p(I-1)(n_max downto 1) or (
        hypo_tmp(n_max downto 1) and (n_max downto 1 => tmp_and_sel(sel_code_q'HIGH)));
    end loop or_loop;    
      -- Watch out hyponode'HIGH may be >>14 easy
    hypodel_d(n_max downto 1) := hypo_mux_p(ncodes)(n_max downto 1);
        hypodel_q <= hypodel_d;

  end process hypo_mux;

end generate if_mux;


no_mux : if ncodes = 1 generate

    hypodel_q(n_max downto 1) <= hypo(1)(n_max downto 1);
  
end generate no_mux;



err_accum_ber_atl : Process (clk, reset)
begin
if reset = '1' then
    err_accum_ber <= (others => '0');
elsif Rising_edge(clk) then
-- I will have to delay sop_source here.
    if ber_clear_q='1' then
        err_accum_ber <= (others => '0');
    --elsif sop_source_del(3)='1' and source_val_del(3)='1' then
    elsif sop_source_del(3)='1' and source_val_del(2)='1' then
        err_accum_ber(log2_n_max downto 1) <= err_add;
        err_accum_ber(numerr_size downto log2_n_max+1) <= (others => '0');
        -- I need to delay outvalid
    --  25-01-2007  simulation shows the source_val_del(2) is the good one to validate data
    --elsif source_val_del(3)='1' then
    elsif source_val_del(2)='1' then 
        err_accum_ber <= unsigned(err_accum_ber) + unsigned(err_add);
    end if;
end if;
end process err_accum_ber_atl;


numerr <= err_accum_ber;


-- ALL of this to a ref design
-- 24 bit counter, resolves BER down to ~10^-7
--time_atl_ber : Process (clk, reset)
--begin
--if reset = '1' then
--    cnt_time_ber <= (others => '0');
--elsif Rising_edge(clk) then
--    if enable='1' then
--        if ber_chk='1' or sync_rot='1' or clr_counters_ber='1' then
--            cnt_time_ber <= (others => '0');
--        elsif outvalid='1' then
--            cnt_time_ber <= cnt_time_ber + natural(1);
--        end if;
--    end if;
--end if;
--end process time_atl_ber;
--
--comp: process(period_ber, cnt_time_ber)
--
--begin
--    if unsigned(period_ber) <= unsigned(cnt_time_ber) then
--        ber_chk <= '1';
--    else
--        ber_chk <= '0';
--    end if;    
--end process comp;
--
--
--ifg6g : if node_sync="used" generate
--
--  FSM: process(state, ns_thr_chk, ns_time_chk)
--
--      begin
--          case state is
--          when initial =>
--              if ns_time_chk='1' then
--                  next_state <= clear_out;
--              else
--                  next_State <= initial;
--              end if;
--          when clear_out =>
--              if ns_thr_chk='1' then
--                  next_state <= outsync;
--              else
--                  next_State <= insync;
--              end if;
--            when insync =>
--              if ns_time_chk='1' then
--                  next_state <= clear_in;
--              else
--                  next_State <= insync;
--              end if;
--            when clear_in =>
--              if ns_thr_chk='1' then
--                  next_state <= outsync;
--              else
--                  next_State <= insync;
--              end if;
--          when outsync =>
--              if ns_time_chk='1' then
--                  next_state <= clear_out;
--              else
--                  next_State <= outsync;
--              end if;
--          when others => next_state <= initial;
--          end case;
--          
--      end process FSM;
--
--  clk_FSM: Process (clk, reset)
--      begin
--          if reset='1' then
--              state <= initial;
--          elsif Rising_edge(clk) then
--                if enable='1' then
--                  state <= next_state;
--                end if;
--          end if;
--          
--  end process clk_FSM;
--
--
--  in_sync <= out_fsm(1);
--  out_sync <= out_fsm(2);
--  clr_counters_ns <= out_fsm(3);
--    clr_counters_ber <= out_fsm(4);
--
--  outputs_FSM: process(state)
--
--      begin
--          case state is
--          when initial =>
--              out_fsm <= "0000";
--            when clear_out =>
--              out_fsm <= "1100";
--            when clear_in =>
--              out_fsm <= "0101";
--          when outsync =>
--              out_fsm <= "0010";
--          when insync =>
--              out_fsm <= "0001";
--          when others => 
--              out_fsm <= "0000";
--          end case;
--          
--  end process outputs_FSM;
--
--
--  err_accum_ns_atl : Process (clk, reset)
--  begin
--  if reset = '1' then
--      err_accum_ns <= (others => '0');
--  elsif Rising_edge(clk) then
--      if enable='1' then
--          if sync_rot='1' or clr_counters_ns='1' then
--              err_accum_ns <= (others => '0');
--          elsif outdel_q(2)='1' then
--              err_accum_ns <= unsigned(err_accum_ns) + unsigned(err_add); --natural(1);
--          end if;
--      end if;
--  end if;
--  end process err_accum_ns_atl;
--
--  numerr_ns <= err_accum_ns;
--
--  comp3: process(threshold_ns, err_accum_ns)
--
--  begin
--      if unsigned(threshold_ns) <= unsigned(err_accum_ns) then
--          ns_thr_chk <= '1';
--      else
--          ns_thr_chk <= '0';
--      end if;    
--  end process comp3;
--
--
--  time_atl_ns : Process (clk, reset)
--  begin
--  if reset = '1' then
--      cnt_time_ns <= (others => '0');
--  elsif Rising_edge(clk) then
--      if enable='1' then
--          if clr_counters_ns='1' or sync_rot='1' then
--              cnt_time_ns <= (others => '0');
--          elsif outvalid='1' then
--              cnt_time_ns <= cnt_time_ns + natural(1);
--          end if;
--      end if;
--  end if;
--  end process time_atl_ns;
--
--  comp2: process(period_ns, cnt_time_ns)
--
--  begin
--      if unsigned(period_ns) <= unsigned(cnt_time_ns) then
--          ns_time_chk <= '1';
--      else
--          ns_time_chk <= '0';
--      end if;    
--  end process comp2;
--
--end generate ifg6g;
--
--
--ratereg : Process (clk, reset)
--begin
--if reset = '1' then
--    ratereg_q <= (others => '0');
--elsif Rising_edge(clk) then
--    if enable='1' and ber_chk='1' then
--        ratereg_q <= err_accum_ber;
--    end if;
--end if;
--end process ratereg;
--                    
--bererr <= ratereg_q;

end architecture rtl;

