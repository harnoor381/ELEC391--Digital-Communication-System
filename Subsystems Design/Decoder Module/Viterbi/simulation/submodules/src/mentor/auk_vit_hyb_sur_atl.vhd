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
-- $Workfile:   auk_vit_hyb_sur_atl_arc_rtl.vhd  $
-- $Archive:   Y:/IP_PVCS/archives/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_sur_atl_arc_rtl.vhd-arc  $
--
-- $RCSfile: auk_vit_hyb_sur_atl_arc_rtl.vhd,v $
-- $Source: /disk2/cvs/data/Projects/Viterbi/Units/hybrid/atlantic/Attic/auk_vit_hyb_sur_atl_arc_rtl.vhd,v $
--
-- $Revision: #1 $
-- $Date: 2008/07/12 $
-- Check in by 	 	 : $Author: max $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description	:  
--
-- ALTERA Confidential and Proprietary
-- Copyright 2002 (c) Altera Corporation
-- All rights reserved
--
-------------------------------------------------------------------------
-------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.vi_interface.all;
use work.vi_functions.all;

Entity auk_vit_hyb_sur_atl is
	Generic (
		L 				: NATURAL := 7;
		ACS_units : NATURAL := 4
	);
	Port (
		clk, reset : in Std_Logic;
 		survtop, survbot : in Std_Logic_Vector(ACS_units downto 1);
 		survive : out Std_Logic_Vector(2**(L-1) downto 1)
	);	
end entity auk_vit_hyb_sur_atl;	


Architecture rtl of auk_vit_hyb_sur_atl is

Constant log2_ACS_units : NATURAL := LOG2_ceil_table(ACS_units);
Constant maxstates : NATURAL := 2**(L-1);
Constant halfstates : NATURAL := 2**(L-2);
Constant states_fractions : NATURAL_ARRAY(0 to log2_ACS_units+1) := 
														Build_two_power_L_list(L, ACS_units);

signal survflop_d, survflop_q : Std_Logic_Vector(2**(L-1) downto 1);

begin

-- make sure that survivor for path 1 ends up in location 1
fg1: FOR j IN 1 TO ACS_units GENERATE
	survflop_d(halfstates-ACS_units+j) <= survtop(j);
	survflop_d(maxstates-ACS_units+j) <= survbot(j);
  fg2: FOR k IN 2 TO states_fractions(log2_ACS_units+1) GENERATE
		survflop_d(halfstates-ACS_units*k+ACS_units-j+1) <=
		survflop_q(halfstates-ACS_units*(k-1)+ACS_units-j+1);
    survflop_d(maxstates-ACS_units*k+ACS_units-j+1) <=
    survflop_q(maxstates-ACS_units*(k-1)+ACS_units-j+1);
  END GENERATE fg2;
END GENERATE fg1;

survflop : Process (clk, reset)
begin
if reset='1' then
	survflop_q <= (others => '0');
elsif Rising_edge(clk) then
--	if go='1' then
		survflop_q <= survflop_d;
--	end if;
end if;
end process survflop;

survive <= survflop_q;
					
end architecture rtl;	
