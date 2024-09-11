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
-- $Workfile:   auk_vit_hyb_bmp_atl_arc_rtl.vhd  $
-- $Archive:   Y:/IP_PVCS/archives/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_bmp_atl_arc_rtl.vhd-arc  $
--
-- $RCSfile: auk_vit_hyb_bmp_atl_arc_rtl.vhd,v $
-- $Source: /disk2/cvs/data/Projects/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_bmp_atl_arc_rtl.vhd,v $
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
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.vi_interface.all;
use work.vi_functions.all;


Entity auk_vit_hyb_bmp_atl is
	Generic (
				n 					: STRING  := "2_2_3_4";
				L 					: STRING  := "3_5_5_5";
				modes				: STRING  := "V_V_V_T";
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
				ga 					: STRING := "5_19_21_21";
				gb 					: STRING := "7_29_27_23";
				gc 					: STRING := "0_0_31_27";
				gd 					: STRING := "0_0_0_31";
				ge 					: STRING := "0_0_0_0";
				gf 					: STRING := "0_0_0_0";
				gg 					: STRING := "0_0_0_0";
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
end entity auk_vit_hyb_bmp_atl;	

Architecture rtl of auk_vit_hyb_bmp_atl is


Constant GND : Std_Logic := '0';
Constant log2_ACS_units : NATURAL := LOG2_ceil_table(ACS_units);
Constant log2_cc : NATURAL := L_max-2-log2_ACS_units;
Constant maxstates : NATURAL := 2**(L_max-1);
Constant bm_size : NATURAL := softbits+LOG2_ceil_table(n_max);
Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
--Constant m_list : NATURAL_ARRAY(1 to ncodes) := Get_modes_list(modes => modes, ncodes => ncodes);
--Constant L_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);
--Constant n_min : NATURAL := Get_n_min(n_list);

-- Because I use binary_table in 2 places I have to size to max(ncodes, n_max);
Constant log2_ncodes : NATURAL := LOG2_ceil_table(ncodes);
--Constant log2_n_max : NATURAL := LOG2_ceil_table(n_max);

--Constant state_node_sync_mod : Std_Logic_Vector(log2_n_max downto 1) :=
--				 natural_2_m(arg => n_max-1, size => log2_n_max);

-- I would have to upgrade this function to take into account 
-- the trellis mode!! This in order to have "node_synch" feature in TCM mode
--Constant ncodes : NATURAL := Get_ncodes(n);
Constant n_max_tcm : NATURAL := Get_n_max_modes(n => n, m => modes);
Constant node_synch_ctrl_matrix : array_of_pla_tables_t(n_max_tcm downto 1) := 
    		 gen_node_sync_mux_ctrl(n => n, m => modes, n_max => n_max_tcm);

Constant bin_tbl_max : NATURAL := Get_max_of_three(n_max_tcm, ncodes, 0);
Constant binary_table : Vector_2D(0 to bin_tbl_max-1) := Build_binary_table(bin_tbl_max);
--Constant sel_mcode : pla_table_t(ncodes downto 1) := 
--				 gen_mcode_selector(n => n_list, n_max => n_max, ncodes => ncodes);


-- Component instantiated by Turbo autoplace on 20/01/2003 at 11:15:40
COMPONENT auk_vit_hyb_sur_atl
	Generic (
		L 				: NATURAL := 7;
		ACS_units : NATURAL := 4
	);
	Port (
		clk, reset : in Std_Logic;
 		survtop, survbot : in Std_Logic_Vector(ACS_units downto 1);
 		survive : out Std_Logic_Vector(2**(L-1) downto 1)
	);	
END COMPONENT;


COMPONENT auk_vit_hyb_met_atl
	Generic (
	  n 					: STRING  := "2_2_3_2";
		L 					: STRING  := "3_5_5_5";
		modes				: STRING  := "V_V_V_T";
		ncodes : NATURAL := 4;
		n_max : NATURAL := 3;
		L_max : NATURAL := 5;
		ACS_units : NATURAL := 1;
		log2_cc   : NATURAL := 3;
		softbits : NATURAL := 4;
		ga 					: STRING := "5_19_21_19";
		gb 					: STRING := "7_29_27_29";
		gc 					: STRING := "0_0_31_0";
		gd 					: STRING := "0_0_0_0";
		ge 					: STRING := "0_0_0_0";
		gf 					: STRING := "0_0_0_0";
		gg 					: STRING := "0_0_0_0"
	);
	Port (
		clk, reset : in Std_Logic;
		sel_code : in Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
		pre_state_vitenca : in Std_Logic_Vector(log2_cc+1 downto 1);
		sym_intf : in Std_Logic_Matrix(n_max downto 1, softbits+1 downto 1);
 		bm_hypo_nil_top, bm_hypo_nil_bot : out Vector_2D(ACS_units downto 1);
		bm_hypo_one_top, bm_hypo_one_bot : out Vector_2D(ACS_units downto 1)
	);	
END COMPONENT;


-- Component instantiated by Turbo autoplace on 11/07/2003 at 19:12:52
COMPONENT auk_vit_hyb_acs_atl
	Generic (
				n : NATURAL := 2;
				L : STRING  := "3_5_5_5";
				L_max : NATURAL := 5;
				ACS_units : NATURAL := 1;
				softbits : NATURAL := 4;
				bmgwide : NATURAL := 12;
				log2_cc : NATURAL := 3;
				ncodes : NATURAL := 4;
				sm_init_logic : STRING := "used";
				dev_family : STRING := "Stratix"
	);
	Port (
		clk, reset : in Std_Logic;
		val_sink, eop_sink : in Std_Logic;
		dav_master_sink : in Std_Logic; -- input in Master mode, output in Slave mode
		ena_master_sink : out Std_Logic; -- output in Master mode, input in Slave mode
        allow_ena_assert_from_trb : in Std_Logic;
		bm_hypo_nil_top : in Vector_2D(ACS_units downto 1);
		bm_hypo_nil_bot : in Vector_2D(ACS_units downto 1);
		bm_hypo_one_top : in Vector_2D(ACS_units downto 1);
		bm_hypo_one_bot : in Vector_2D(ACS_units downto 1);

		-- what about these two? Do someone REALLY needs this? Maybe.
		-- specially the bm_init_state. Definitely an option is required to 
		-- generate the logic for bm_init_state.
		bm_init_state : in Std_Logic_Vector(L_max-1 downto 1);
		bm_init_value : in Std_Logic_Vector(bmgwide downto 1);
		tr_init_state : in Std_Logic_Vector(L_max-1 downto 1);
		sel_code : in Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);

 		survivetop, survivebot : out Std_Logic_Vector(ACS_units downto 1);
		pre_state_vitenca : out Std_Logic_Vector(log2_cc+1 downto 1);
 		survready_q : out Std_Logic;
		check_dav_sink, latch_best_mark : out Std_Logic;
		normalizations : out Std_Logic_Vector(8 downto 1);
		bestmet : out Std_Logic_Vector(bmgwide downto 1);
		bestadd : out Std_Logic_Vector(L_max-1 downto 1);
		block_delimeters : out Std_Logic
	);	
END COMPONENT;



Subtype vector_so is Std_Logic_Vector(softbits+1 downto 1);
Subtype Vector_2D_ACS is Vector_2D(ACS_units downto 1);

type matrix_so is array(NATURAL RANGE <>) of vector_so;
Type Vector_3D is ARRAY(NATURAL RANGE<>) of Vector_2D_ACS;


signal pla_mux_ctrl_in : Std_Logic_Vector(log2_ncodes+log2_n_max downto 1);
signal state_node_sync_q : Std_Logic_Vector(log2_n_max downto 1);
signal norm_ocurred : Std_Logic_Vector(8 downto 1);
signal period : Std_Logic_Vector(16 downto 1);
signal rxbits, rxzip, sel_decoded : Std_Logic_Vector(n_max downto 1);
signal leftadd : Std_Logic_Vector(log2_cc+1 downto 1);
signal delen_d, delen_q : Std_Logic_Vector(4 downto 1);  -- ??
signal rrff_d, rrff_q, rrffa_d : matrix_so(n_max downto 1);
signal rrffa_q : Std_Logic_Matrix(n_max downto 1, softbits+1 downto 1);

signal sa, sb : Std_Logic_Vector(ACS_units downto 1);
signal survivors : Std_Logic_Vector(maxstates downto 1);
--signal survready_q : Std_Logic;

--Signal dav_sink_q, 
Signal ena_slave_source_q : Std_Logic; 
Signal allow_ena_assert, ena_assert_mark : Std_Logic; 

Signal val_sink_q, check_dav_sink : Std_Logic; 
Signal bm_init_state_q : Std_Logic_Vector(L_max-1 downto 1);
Signal bm_init_value_q : Std_Logic_Vector(bmgwide downto 1);
signal sel_code_q : Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
signal pre_state_vitenca : Std_Logic_Vector(log2_cc+1 downto 1);
signal bm_hypo_nil_top, bm_hypo_nil_bot : Vector_2D(ACS_units downto 1);
signal bm_hypo_one_top, bm_hypo_one_bot : Vector_2D(ACS_units downto 1);


begin


-- registering atlantic ports

clk_atl: Process (clk, reset)
	begin
		if reset='1' then
			val_sink_q <= '0'; -- to remain here
		elsif Rising_edge(clk) then
			val_sink_q <= val_sink;  
		end if;
		
end process clk_atl;


-----------------------------------------

node_sync_logic2: if node_sync="used" generate

state_node_sync_q <= state_node_sync;

-- I have to make this counter to rotate modulus of n_max
--  state_node_sync_atl : Process(clk, reset)
--  begin
--  if reset='1' then
--    state_node_sync_q <= (others => '0');
--  elsif Rising_edge(clk) then
--    if val_sink='1' then
--     	state_node_sync_q <= state_node_sync;
--    end if;
--  end if;
--  end process state_node_sync_atl;          

end generate node_sync_logic2;

input_pla1: if node_sync="used" and ncodes=1 generate
	pla_mux_ctrl_in <= state_node_sync_q;
end generate input_pla1;

input_pla2: if node_sync="used" and ncodes>1 generate
	pla_mux_ctrl_in <= sel_code_q&state_node_sync_q;
end generate input_pla2;

node_sync_logic: if node_sync="used" generate

  g2: for K in 1 to n_max generate

    pla_2_mux: process(pla_mux_ctrl_in, rrff_q)  -- I'll fit the mux as well

  			variable pla_mux_sel : Std_Logic_Vector(log2_n_max downto 1); 
  			variable rrff_mux_p :	Vector_2D(n_max downto 0);  
  			variable tmp_and_sel : Std_Logic_Vector(log2_n_max downto 0);
    begin
			pla_table ( pla_mux_ctrl_in, pla_mux_sel, node_synch_ctrl_matrix(K));
      tmp_and_sel(0) := '1';
      rrff_mux_p(0)(softbits+1 downto 1) := (others => '0');
      or_loop: For I in 1 to n_max loop
      	and_loop: For J in 1 to log2_n_max loop
      		if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
      			tmp_and_sel(J) := tmp_and_sel(J-1) and not pla_mux_sel(J);
      		else  -- bit J of I-1 is 1
      			tmp_and_sel(J) := tmp_and_sel(J-1) and pla_mux_sel(J);
      		end if;
      	end loop and_loop;
      	rrff_mux_p(I)(softbits+1 downto 1) :=
      	rrff_mux_p(I-1)(softbits+1 downto 1) or (
      	rrff_q(I)(softbits+1 downto 1) and (softbits+1 downto 1 => tmp_and_sel(log2_n_max)));
      end loop or_loop;
    	rrffa_d(k)(softbits+1 downto 1) <= rrff_mux_p(n_max)(softbits+1 downto 1);
    end process pla_2_mux;

  end generate g2;

end generate node_sync_logic;


fg3: FOR k IN 1 TO n_max GENERATE
  rrff_d(k)(softbits downto 1) <= rr(k*softbits downto ((k-1)*softbits+1));
	rrff_d(k)(softbits+1) <= eras_sym(k);
	ifg3b: if node_sync="unused" generate
  	rrffa_d(k) <= rrff_q(k);
	end generate ifg3b;
END GENERATE fg3;

-- done higher up
bm_init_value_q <= bm_init_value;
bm_init_state_q <= bm_init_state;
sel_code_q <= sel_code;

rrff : Process (clk, reset)
begin
if reset = '1' then
	rrff_q <= (others => (others => '0'));
--	bm_init_state_q <= (others => '0');
--	bm_init_value_q <= (others => '0');
--	sel_code_q <= (others => '0');
elsif Rising_edge(clk) then
	-- I have to keep it out fo val_sink because I need it just after reset
--	bm_init_value_q <= bm_init_value;
	if val_sink='1' then
		rrff_q <= rrff_d;
--		bm_init_state_q <= bm_init_state;
		
--		sel_code_q <= sel_code;
	end if;
end if;
end process rrff;



-- survready is a signal going to the trb to indicate 
-- survivors are ready. Now it will be generated by the FSM
-- from bmp
			

rrffa : Process (clk, reset)
begin
if reset = '1' then
	--rrffa_q <= (others => '0', others => '0');
	rrffa_q <= (others => (others => '0'));
elsif Rising_edge(clk) then
	if val_sink_q='1' then
		for I in 1 to n_max loop
			for J in 1 to softbits+1 loop
				rrffa_q(I, J) <= rrffa_d(I)(J);
			end loop;
		end loop;
	end if;
end if;
end process rrffa;

--------------

-- this logic comes from BER
if_ber : if BER="used" generate

  fg2: for j in 1 to n_max generate
	-- moving form a_q to a_d 
    rxbits(j) <= rrffa_d(j)(softbits);
		rxzip(j) <= not rrffa_d(j)(softbits+1);
  end generate fg2;

  data_for_BER(2*n_max downto n_max+1) <= rxzip;
  data_for_BER(n_max downto 1) <= rxbits;

end generate if_ber;

if_notber: if BER="unused" generate
    data_for_BER <= (others => '0');
end generate if_notber;


metric_calc: auk_vit_hyb_met_atl
	Generic map (n => n, L => L, modes => modes, ncodes => ncodes, 
							 n_max => n_max, L_max => L_max, log2_cc => log2_cc, 
							 softbits => softbits, ACS_units => ACS_units,
							 ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)
	Port map (clk => clk, reset => reset, 
						sel_code => sel_code_q,
						pre_state_vitenca => pre_state_vitenca,
						sym_intf => rrffa_q, 

						bm_hypo_nil_top => bm_hypo_nil_top, 
						bm_hypo_nil_bot => bm_hypo_nil_bot,
						bm_hypo_one_top => bm_hypo_one_top, 
						bm_hypo_one_bot => bm_hypo_one_bot);



path_selection: auk_vit_hyb_acs_atl
	Generic map (n => n_max, L => L, L_max => L_max, softbits => softbits,
							ACS_units => ACS_units,	bmgwide => bmgwide, log2_cc => log2_cc,
							ncodes => ncodes, sm_init_logic => sm_init_logic,
							dev_family => dev_family)
	Port map (clk => clk, reset => reset, 
						val_sink => val_sink, eop_sink => eop_sink,
						dav_master_sink => dav_master_sink,
						ena_master_sink => ena_master_sink,
						allow_ena_assert_from_trb => allow_ena_assert_from_trb,
						check_dav_sink => check_dav_sink,
						bm_init_state => bm_init_state_q,
						bm_init_value => bm_init_value_q,
						sel_code => sel_code_q,
						bestmet => bestmet,
						bestadd => bestadd,
						tr_init_state => tr_init_state,
						pre_state_vitenca => pre_state_vitenca,
						bm_hypo_nil_top => bm_hypo_nil_top, 
						bm_hypo_nil_bot => bm_hypo_nil_bot,
						bm_hypo_one_top => bm_hypo_one_top, 
						bm_hypo_one_bot => bm_hypo_one_bot,
 						survivetop => sa, survivebot => sb, survready_q => survready,
 						normalizations => normalizations, latch_best_mark => latch_best_mark,
						block_delimeters => block_delimeters );

--*****************
--*** SURVIVORS ***
--*****************

-- latch every clock cycle

store_survivors : auk_vit_hyb_sur_atl
	Generic map (L => L_max, ACS_units => ACS_units)
	Port map (clk => clk, reset => reset,
 						survtop => sa, survbot => sb, 
 						survive => survivors);

--***************
--*** OUTPUTS ***
--***************
survive <= not survivors;
--we_ram_sur_out <= we_ram_sur;

-- Readability

--fg_read: For I in 1 to ACS_units generate
--if_Read1 : if BMG_MAX > bmgwide generate

--	mettop_mux_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	metbot_mux_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	hypo_nil_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	hypo_one_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--  writeadd_odd(I)(BMG_MAX downto seq_width+1) <= (others => '0');
--  writeadd_even(I)(BMG_MAX downto seq_width+1) <= (others => '0');
--	data_odd_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	data_even_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');

--end generate if_read1;

--end generate fg_read;

end architecture rtl;

