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
-- $Workfile:   auk_vit_hyb_ber_atl_arc_rtl.vhd  $
-- $Archive:   Y:/IP_PVCS/archives/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_ber_atl_arc_rtl.vhd-arc  $
--
-- $RCSfile: auk_vit_hyb_ber_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_ber_atl_arc_rtl.vhd,v $
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
library altera_mf;
use altera_mf.altera_mf_components.all;


Entity auk_vit_hyb_ber_atl is
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
end entity auk_vit_hyb_ber_atl;    


Architecture rtl of auk_vit_hyb_ber_atl is


Constant GND : Std_Logic := '0';
Constant VCC : Std_Logic := '1';

-- To be removed but ...
Constant bitsout_max : NATURAL := 1;

Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant L_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);
Constant binary_table : Vector_2D(0 to ncodes-1) := Build_binary_table(ncodes);

Constant two_pow_bitsout_max : NATURAL := 2;
Constant log2_max : NATURAL := LOG2_ceil_Table(n_max+1);
Constant count_limit : std_logic_vector(vlog_wide downto 1) := --natural_2_m(arg => 2**vlog_wide - 2, size => vlog_wide);
     CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**vlog_wide - 2, SIZE => vlog_wide), SIZE => vlog_wide);
-- Do I need this? [26/07/2004]  NO
--Constant num_bit_err_matrix : pla_table_t(two_pow_bitsout_max downto 1) :=
                 --gen_num_bit_errors(m => 1);



-- Component instantiated by Turbo autoplace on 14/07/2003 at 19:42:19
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


signal readadd, writeadd, symbol_count : Std_Logic_Vector(vlog_wide downto 1);
signal err_accum_ber : Std_Logic_Vector(numerr_size downto 1);
signal sel_code_q : Std_Logic_Vector(sel_code_size downto 1);
signal err_add : Std_Logic_Vector(log2_max downto 1);
signal sout : Std_Logic_Vector(2*n_max downto 1);
signal hypo : Vector_2D(1 to ncodes);

--  the previous signal do use a 1D to big , they are going to need 
-- an special vector_2D definition.
signal err, hypodel_q : Std_Logic_Vector(n_max downto 1);
signal shift_d, shift_q : Std_Logic_Vector(L_max downto 1);
signal sink_val_q : Std_Logic;
Signal source_val_del : Std_Logic_Vector(2 downto 1);
signal ber_clear_q : std_logic;




begin



sink_val_clk : Process (clk, reset)
begin
if reset = '1' then
    sink_val_q <= '0';
    source_val_del <= (others => '0');
    sel_code_q <= (others => '0');
    ber_clear_q <= '0';
elsif Rising_edge(clk) then
    if sop_source='1' then
        sel_code_q <= sel_code;
    end if;
    ber_clear_q <= ber_clear;
    sink_val_q <= sink_val;
    source_val_del(1) <= outvalid;
    source_val_del(2) <= source_val_del(1);
end if;
end process sink_val_clk;


-- if moving data_ram_in process to _bmp then I have to connect this load to a 
--  different signal coming from bmp to store data_ram_in from rrffa.

counters_atl : Process (clk, reset)
begin
if reset = '1' then
    writeadd <= (others => '0');
    readadd <= (others => '0');
    symbol_count <= (others => '0');
elsif Rising_edge(clk) then
    if sink_val_q='1' then
        writeadd <= writeadd + natural(1);
    end if;
    if outvalid='1' then
        readadd <= readadd + natural(1);
    end if;
    if sink_val_q='1' and outvalid='0' then
        symbol_count <= unsigned(symbol_count) + natural(1);
    elsif sink_val_q='0' and outvalid='1' then
        symbol_count <= unsigned(symbol_count) - natural(1);
    end if;
end if;
end process counters_atl;

mem_ctrl : process(symbol_count)
begin
    if (unsigned(symbol_count) > unsigned(count_limit)) then 
        allow_ena_assert <= '0';
    else
        allow_ena_assert <= '1';
    end if;
end process mem_ctrl;


--ifg1: IF dev_family="STRATIX" or dev_family="stratix" or dev_family="Stratix" GENERATE

    sout_ram: altsyncram 
   GENERIC map (
      operation_mode => "DUAL_PORT", width_a => 2*n_max, widthad_a => vlog_wide, numwords_a => 2**vlog_wide,
      outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
      outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
      wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, address_reg_b => "CLOCK0",
            width_b => 2*n_max, widthad_b => vlog_wide, numwords_b => 2**vlog_wide,
            rdcontrol_reg_b => "UNUSED",
            outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
            indata_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
            indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
      read_during_write_mode_mixed_ports => "DONT_CARE", ram_block_type => "AUTO",
      intended_device_family => dev_family)
   PORT map (
      wren_a => sink_val_q, data_a => data_ram_in, address_a => writeadd,
            address_b => readadd, clock0 => clk, 
            --aclr0 => reset, 
      q_b => sout );

--end generate ifg1;

-- To be moved to inside a process and shift_d converted into a variable

-- use hypodel_q instead of hypo because of muxing
fg7: for k in 1 to n_max generate
  err(k) <= (sout(k) xor hypodel_q(k)) and sout(n_max+k);
end generate fg7;

calc_errors: auk_vit_add_tre
    Generic map (n => n_max)
    Port map (diffs => err,    errs => err_add );

--- end from parallel

shift_d(L_max) <= decbit;
shift_d(L_max-1 downto 1) <= shift_q(L_max downto 2) and (L_max downto 2 => not sop_source);

shift : Process (clk, reset)
begin
if reset = '1' then
    shift_q <= (others => '0');
elsif Rising_edge(clk) then
    if outvalid='1' then
        shift_q <= shift_d;
    -- elsif ber_clear_q='1' then
        -- shift_q <= (others => '0');
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



--- here I need a big generic MUX to get the ncodes hyponodes downto hypodel

-- I think I can remove the FF at the end of the MUX 

if_mux : if ncodes > 1 generate

  hypo_mux : Process(clk, reset)
--    hypo_mux : Process(hypo, sel_code)

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
--        hypodel_q <= hypodel_d;

    if reset = '1' then
        hypodel_q <= (others => '0');
    elsif Rising_edge(clk) then
           hypodel_q <= hypodel_d;
    end if;

  end process hypo_mux;

end generate if_mux;


no_mux : if ncodes = 1 generate

--    hypodel_q(n_max downto 1) <= hypo(1)(n_max downto 1);

  hypo_no_mux : Process(clk, reset)

--      variable hypodel_d : Std_Logic_Vector(n_max downto 1);

  begin
    -- Watch out hyponode'HIGH may be >>14 easy
--    hypodel_d(n_max downto 1) := hypo(1)(n_max downto 1);
    if reset = '1' then
        hypodel_q <= (others => '0');
    elsif Rising_edge(clk) then
           hypodel_q <= hypo(1)(n_max downto 1);
    end if;

  end process hypo_no_mux;

end generate no_mux;

                    

err_accum_ber_atl : Process (clk, reset)
begin
if reset = '1' then
    err_accum_ber <= (others => '0');
elsif Rising_edge(clk) then
    if sop_source='1' or ber_clear_q='1' then
        err_accum_ber <= (others => '0');
        -- I need to delay outvalid
    elsif source_val_del(2)='1' then -- err_add ougth to be zero at times
        err_accum_ber <= unsigned(err_accum_ber) + unsigned(err_add);
    end if;
end if;
end process err_accum_ber_atl;


numerr <= err_accum_ber;


end architecture rtl;    
