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
-- $Workfile:   auk_vit_hyb_met_atl_arc_rtl.vhd  $
-- $Archive:   Y:/IP_PVCS/archives/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_met_atl_arc_rtl.vhd-arc  $
--
-- $RCSfile: auk_vit_hyb_met_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/hybrid/atlantic/auk_vit_hyb_met_atl_arc_rtl.vhd,v $
--
-- $Revision: #1 $
-- $Date: 2008/07/12 $
-- Check in by 	 	 : $Author: max $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description	:  metric calculation for the hybrid decoder
--                 This revision has new quantification (even levels) scheme
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


Entity auk_vit_hyb_met_atl is
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
end entity auk_vit_hyb_met_atl;	


Architecture rtl of auk_vit_hyb_met_atl is

Constant VCC : Std_Logic := '1';
Constant softbits_one : Std_Logic_Vector(softbits downto 1) :=
				 --natural_2_m(arg => 1, size => softbits);
				 CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => softbits), SIZE => softbits);
Constant log2_ACS_units : NATURAL := LOG2_ceil_table(ACS_units);
constant bm_size : NATURAL := softbits+LOG2_ceil_table(n_max);
Constant bin_tbl_max : NATURAL := Get_max_of_three(n_max, ncodes, 0);
Constant binary_table : Vector_2D(0 to bin_tbl_max-1) := Build_binary_table(bin_tbl_max);
Constant L_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);
Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant m_list : NATURAL_ARRAY(1 to ncodes) := Get_modes_list(modes => modes, ncodes => ncodes);


COMPONENT auk_vit_var_enc
	Generic (
		n : NATURAL := 2;
		L_max : NATURAL := 7;
		L_code	: NATURAL  := 7;
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

-- vector_1D_bmsize to be replaced by vector_bm  
--Subtype vector_1D_bmsize is Std_Logic_Vector(bm_size downto 1);
Subtype vector_bm is Std_Logic_Vector(bm_size downto 1);
Subtype vector_lm is Std_Logic_Vector(L_max downto 1);
Subtype vector_nm is Std_Logic_Vector(n_max downto 1);
Subtype vector_so is Std_Logic_Vector(softbits+1 downto 1);


-- matrix_2D to be replaced by matrix_bm
--type matrix_2D is array(NATURAL RANGE <>) of vector_bm;
type matrix_bm is array(NATURAL RANGE <>) of vector_bm;
type matrix_lm is array(NATURAL RANGE <>) of vector_lm;
type matrix_nm is array(NATURAL RANGE <>) of vector_nm;
type matrix_so is array(NATURAL RANGE <>) of vector_so;

-- matrix_2D_n_bmsize to be replaced by matrix_nm_bm
--Subtype matrix_2D_n_bmsize is matrix_2D(n_max downto 1);
Subtype matrix_nm_bm is matrix_bm(n_max downto 1);
Subtype matrix_ac_bm is matrix_bm(ACS_units downto 1);
Subtype matrix_ac_lm is matrix_lm(ACS_units downto 1);
Subtype matrix_ac_nm is matrix_nm(ACS_units downto 1);

-- matrix_3D to be replaced by cube_nm_bm
--Type matrix_3D is array(NATURAL RANGE <>) of matrix_2D_n_bmsize;
Type cube_nm_bm is array(NATURAL RANGE <>) of matrix_nm_bm;
-- to replace Vector_3D for topmetric
Type cube_ac_bm is array(NATURAL RANGE <>) of matrix_ac_bm;
-- to replace vector_3D for state_vitenca
Type cube_ac_lm is array(NATURAL RANGE <>) of matrix_ac_lm;
Type cube_ac_nm is array(NATURAL RANGE <>) of matrix_ac_nm;

-- a cube is a 3D array
Subtype cube_ac_nm_bm is cube_nm_bm(ACS_units downto 1);
Subtype cube_nc_ac_nm is cube_ac_nm(ncodes downto 1);
-- metacube is a 4D array
Type metacube_ac_nm_bm is array(NATURAL RANGE <>) of cube_ac_nm_bm;
-- to replace Vector_4D for hypo
Type metacube_nc_ac_nm is array(NATURAL RANGE <>) of cube_nc_ac_nm;

--Subtype Vector_2D_ACS is Vector_2D(ACS_units downto 1);
--Type Vector_3D is ARRAY(NATURAL RANGE <>) of Vector_2D_ACS;
--Subtype Vector_3D_ncodes is Vector_3D(ncodes downto 1);
--Type Vector_4D is ARRAY(NATURAL RANGE <>) of Vector_3D_ncodes;

Signal bm_corrtop, bm_corrbot : metacube_ac_nm_bm(ncodes downto 1);
signal sym : matrix_so(n_max downto 1);
--signal state_vitenca : Vector_3D(3 downto 0);
signal state_vitenca : cube_ac_lm(3 downto 0);
-- vector => hypo(B)(I)(K)(n_list(I) downto 1)
--  K = ACS , I = ncodes
--signal hypo : Vector_4D(3 downto 0);
signal hypo : metacube_nc_ac_nm(3 downto 0);
--signal topmetric, botmetric : Vector_3D(ncodes downto 1);
signal topmetric, botmetric : cube_ac_bm(ncodes downto 1);
--signal bm_hypo_nil_top_premux, bm_hypo_nil_bot_premux : Vector_3D(ncodes downto 1);
--signal bm_hypo_one_top_premux, bm_hypo_one_bot_premux : Vector_3D(ncodes downto 1);
signal bm_hypo_nil_top_premux, bm_hypo_nil_bot_premux : cube_ac_bm(ncodes downto 1);
signal bm_hypo_one_top_premux, bm_hypo_one_bot_premux : cube_ac_bm(ncodes downto 1);
--signal bm_hypo_nil_top_d, bm_hypo_nil_bot_d : Vector_2D(ACS_units downto 1);
--signal bm_hypo_one_top_d, bm_hypo_one_bot_d : Vector_2D(ACS_units downto 1);
signal bm_hypo_nil_top_d, bm_hypo_nil_bot_d : matrix_bm(ACS_units downto 1);
signal bm_hypo_one_top_d, bm_hypo_one_bot_d : matrix_bm(ACS_units downto 1);


begin


fg_nmax : for I in 1 to n_max generate		
	fg_soft : for K in 1 to softbits+1 generate
		sym(I)(K) <= sym_intf(I, K);
	end generate fg_soft;
end generate fg_nmax;



-- perhaps the B loop generate is not needed after all
-- or perhaps only need it if there is at least 1 TCM mode ... Will see
fg44 :  For B in 0 to 3 generate -- B for branch
-- 0 -> is bm_hypo_nil_top
-- 1 -> is bm_hypo_nil_bot
-- 2 -> is bm_hypo_one_top
-- 3 -> is bm_hypo_one_bot
	fg33 :  For K in 1 to ACS_units generate

		ifg55 : if B=0 or B=2 generate
			state_vitenca(B)(K)(1) <= '0';
		end generate ifg55;
		ifg56 : if B=1 or B=3 generate
			state_vitenca(B)(K)(1) <= '1';
		end generate ifg56;
		ifg33: if ACS_units>1 generate
			state_vitenca(B)(K)(log2_ACS_units+1 downto 2) <= --natural_2_m(arg => K-1, size => log2_ACS_units);
			    CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => K-1, SIZE => log2_ACS_units), SIZE => log2_ACS_units);
		end generate ifg33;
	 -- pre_state_vitenca only used log2_cc .. 1 however it comes from acs as (log2_cc+1 .. 1)
		state_vitenca(B)(K)(L_max-1 downto log2_ACS_units+2) <= pre_state_vitenca(log2_cc downto 1);
		ifg57 : if B=0 or B=1 generate
			state_vitenca(B)(K)(L_max) <= '0';
		end generate ifg57;
		ifg58 : if B=2 or B=3 generate
			state_vitenca(B)(K)(L_max) <= '1';
		end generate ifg58;
		

	  fg3a: For I in 1 to ncodes generate

	   	encoding: auk_vit_var_enc
	   		Generic map (n => n_list(I), L_max => L_max, L_code => L_list(I), pol_sel => I, 
	   								 ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)
	   		Port map (state => state_vitenca(B)(K)(L_max downto 1),
	   						vector => hypo(B)(I)(K)(n_list(I) downto 1) );
	   	
	  end generate fg3a;
	end generate fg33;
end generate fg44;


fg_nco : for I in 1 to ncodes generate		
fg_acs : for K in 1 to ACS_units generate		

-- This bit of code will have to be generate ncodes times but only if the mode is Viterbi

if_vit_mode: if m_list(I)=0 generate
	branch_metric_calc : Process (hypo, sym)

		--variable corrtop, corrbot : matrix_2D(n_list(I) downto 1);
		variable corrtop, corrbot : matrix_bm(n_list(I) downto 1);

-- Here lies the kernel of the problem: presumption of reversed value for top and bot branch metrics
-- this doesn't hold for TCM.	I need 4 branch metrics here when TCM.
-- however for L_max > L(I) this still can be used.

	begin
		clock_j_n: For J in 1 to n_list(I) loop
			if (hypo(0)(I)(K)(J)='1') then
				corrtop(J)(softbits-1 downto 1) := not (sym(J)(softbits-1 downto 1) or (softbits-1 downto 1 => sym(J)(softbits+1)));
				corrtop(J)(softbits) := sym(J)(softbits) and not sym(J)(softbits+1);
				corrbot(J)(softbits-1 downto 1) := sym(J)(softbits-1 downto 1) and (softbits-1 downto 1 => not sym(J)(softbits+1));
				corrbot(J)(softbits) := not (sym(J)(softbits) or sym(J)(softbits+1));
			else  --hypo(0)(I)(K)(J)='0' 
				corrtop(J)(softbits-1 downto 1) := sym(J)(softbits-1 downto 1) and (softbits-1 downto 1 => not sym(J)(softbits+1));
				corrtop(J)(softbits) := not (sym(J)(softbits) or sym(J)(softbits+1));
				corrbot(J)(softbits-1 downto 1) := not (sym(J)(softbits-1 downto 1) or (softbits-1 downto 1 => sym(J)(softbits+1)));
				corrbot(J)(softbits) := sym(J)(softbits) and not sym(J)(softbits+1);
			end if;
		end loop clock_j_n;

		bm_corrtop(I)(K)(n_list(I) downto 1) <= corrtop(n_list(I) downto 1);
		bm_corrbot(I)(K)(n_list(I) downto 1) <= corrbot(n_list(I) downto 1);

	end process branch_metric_calc;
end generate if_vit_mode;

if_tcm_mode: if m_list(I)=1 generate

-- in TCM mode the sym will be connected / muxed straigth to the branch
-- metric depending on the hypothesis with just 1 clocking stage

    latch_bm_from_input : Process(clk, reset)

    begin
			if reset='1' then
				bm_hypo_nil_top_premux(I)(K)(softbits downto 1) <= (others => '0');
				bm_hypo_nil_bot_premux(I)(K)(softbits downto 1) <= (others => '0');
				bm_hypo_one_top_premux(I)(K)(softbits downto 1) <= (others => '0');
				bm_hypo_one_bot_premux(I)(K)(softbits downto 1) <= (others => '0');
	    elsif Rising_edge(clk) then
				if (hypo(0)(I)(K)(2 downto 1)="00") then
					bm_hypo_nil_top_premux(I)(K)(softbits downto 1) <= sym(1)(softbits downto 1);
				elsif (hypo(0)(I)(K)(2 downto 1)="01") then
					bm_hypo_nil_top_premux(I)(K)(softbits downto 1) <= sym(2)(softbits downto 1);
				elsif (hypo(0)(I)(K)(2 downto 1)="10") then
					bm_hypo_nil_top_premux(I)(K)(softbits downto 1) <= sym(3)(softbits downto 1);
				elsif (hypo(0)(I)(K)(2 downto 1)="11") then
					bm_hypo_nil_top_premux(I)(K)(softbits downto 1) <= sym(4)(softbits downto 1);
				end if;
				if (hypo(1)(I)(K)(2 downto 1)="00") then
					bm_hypo_nil_bot_premux(I)(K)(softbits downto 1) <= sym(1)(softbits downto 1);
				elsif (hypo(1)(I)(K)(2 downto 1)="01") then
					bm_hypo_nil_bot_premux(I)(K)(softbits downto 1) <= sym(2)(softbits downto 1);
				elsif (hypo(1)(I)(K)(2 downto 1)="10") then
					bm_hypo_nil_bot_premux(I)(K)(softbits downto 1) <= sym(3)(softbits downto 1);
				elsif (hypo(1)(I)(K)(2 downto 1)="11") then
					bm_hypo_nil_bot_premux(I)(K)(softbits downto 1) <= sym(4)(softbits downto 1);
				end if;
				if (hypo(2)(I)(K)(2 downto 1)="00") then
					bm_hypo_one_top_premux(I)(K)(softbits downto 1) <= sym(1)(softbits downto 1);
				elsif (hypo(2)(I)(K)(2 downto 1)="01") then
					bm_hypo_one_top_premux(I)(K)(softbits downto 1) <= sym(2)(softbits downto 1);
				elsif (hypo(2)(I)(K)(2 downto 1)="10") then
					bm_hypo_one_top_premux(I)(K)(softbits downto 1) <= sym(3)(softbits downto 1);
				elsif (hypo(2)(I)(K)(2 downto 1)="11") then
					bm_hypo_one_top_premux(I)(K)(softbits downto 1) <= sym(4)(softbits downto 1);
				end if;
				if (hypo(3)(I)(K)(2 downto 1)="00") then
					bm_hypo_one_bot_premux(I)(K)(softbits downto 1) <= sym(1)(softbits downto 1);
				elsif (hypo(3)(I)(K)(2 downto 1)="01") then
					bm_hypo_one_bot_premux(I)(K)(softbits downto 1) <= sym(2)(softbits downto 1);
				elsif (hypo(3)(I)(K)(2 downto 1)="10") then
					bm_hypo_one_bot_premux(I)(K)(softbits downto 1) <= sym(3)(softbits downto 1);
				elsif (hypo(3)(I)(K)(2 downto 1)="11") then
					bm_hypo_one_bot_premux(I)(K)(softbits downto 1) <= sym(4)(softbits downto 1);
				end if;
	    end if;
  	end process latch_bm_from_input;


	bm_hypo_nil_top_premux(I)(K)(bm_size downto softbits+1) <= (others => '0'); 
  bm_hypo_nil_bot_premux(I)(K)(bm_size downto softbits+1) <= (others => '0'); 
	bm_hypo_one_top_premux(I)(K)(bm_size downto softbits+1) <= (others => '0'); 
	bm_hypo_one_bot_premux(I)(K)(bm_size downto softbits+1) <= (others => '0'); 

end generate if_tcm_mode;


--*******************************
--***							***
--***			N = 2			***
--***							***
--*******************************

ifg_n2: IF (n_list(I)=2) and (m_list(I)=0) GENERATE  -- n=2 and viterbi mode

	clocking2: Process(clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin

		if reset = '1' then
			topmetric(I)(K)(softbits+1 downto 1) <= (others => '0');
			botmetric(I)(K)(softbits+1 downto 1) <= (others => '0');
		elsif Rising_edge(clk) then

			addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
			addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
			topmetric(I)(K)(softbits+1 downto 1) <= addtop_d(2)(softbits+1 downto 1);
			botmetric(I)(K)(softbits+1 downto 1) <= addbot_d(2)(softbits+1 downto 1);

		end if;

	end process clocking2;

	ifg_bm_soft2: if bm_size > softbits+1 generate
		topmetric(I)(K)(bm_size downto softbits+2) <= (others => '0');
		botmetric(I)(K)(bm_size downto softbits+2) <= (others => '0');
	end generate ifg_bm_soft2;

end generate ifg_n2;

--*******************************
--***							***
--***			N = 3			***
--***							***
--*******************************

ifg_n3: IF (n_list(I)=3) and (m_list(I)=0) GENERATE

	clocking3 : Process (clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin
	if reset = '1' then
		topmetric(I)(K)(softbits+2 downto 1) <= (others => '0');
		botmetric(I)(K)(softbits+2 downto 1) <= (others => '0');
	elsif Rising_edge(clk) then

		addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
		addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
		addtop_d(3)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(2)(softbits+1 downto 1) ) +
									 unsigned(bm_corrtop(I)(K)(3)(softbits downto 1) );
		addbot_d(3)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(2)(softbits+1 downto 1) ) +
									 unsigned(bm_corrbot(I)(K)(3)(softbits downto 1) );

			topmetric(I)(K)(softbits+2 downto 1) <= addtop_d(3)(softbits+2 downto 1);				
			botmetric(I)(K)(softbits+2 downto 1) <= addbot_d(3)(softbits+2 downto 1);
	end if;
	end process clocking3;

	ifg_bm_soft3: if bm_size > softbits+2 generate
		topmetric(I)(K)(bm_size downto softbits+3) <= (others => '0');
		botmetric(I)(K)(bm_size downto softbits+3) <= (others => '0');
	end generate ifg_bm_soft3; 

end generate ifg_n3;


--*******************************
--***							***
--***			N = 4			***
--***							***
--*******************************


ifg_n4: IF (n_list(I)=4) and (m_list(I)=0) GENERATE


	clocking4 : Process (clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin
	if reset = '1' then
		topmetric(I)(K)(softbits+2 downto 1) <= (others => '0');
		botmetric(I)(K)(softbits+2 downto 1) <= (others => '0');
	elsif Rising_edge(clk) then

		addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
		addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
		addtop_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(4)(softbits downto 1) );
		addbot_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(4)(softbits downto 1) );
		addtop_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addtop_d(3)(softbits+1 downto 1) );
		addbot_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addbot_d(3)(softbits+1 downto 1) );				
			
			topmetric(I)(K)(softbits+2 downto 1) <= addtop_d(4)(softbits+2 downto 1);
			botmetric(I)(K)(softbits+2 downto 1) <= addbot_d(4)(softbits+2 downto 1);

	end if;
	end process clocking4;

	ifg_bm_soft4: if bm_size > softbits+2 generate
		topmetric(I)(K)(bm_size downto softbits+3) <= (others => '0');
		botmetric(I)(K)(bm_size downto softbits+3) <= (others => '0');
	end generate ifg_bm_soft4;

end generate ifg_n4;


----*******************************
----***							***
----***			N = 5			***
----***							***
----*******************************


ifg_n5: IF (n_list(I)=5) and (m_list(I)=0) GENERATE


	clocking5 : Process (clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin
	if reset = '1' then
		topmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
		botmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
	elsif Rising_edge(clk) then

		addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
		addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
		addtop_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(4)(softbits downto 1) );
		addbot_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(4)(softbits downto 1) );
		addtop_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addtop_d(3)(softbits+1 downto 1) );
		addbot_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addbot_d(3)(softbits+1 downto 1) );				
		addtop_d(5)(softbits+3 downto 1) :=
									 unsigned('0' & addtop_d(4)(softbits+2 downto 1) ) +
									 unsigned(bm_corrtop(I)(K)(5)(softbits downto 1) );
		addbot_d(5)(softbits+3 downto 1) :=
									 unsigned('0' & addbot_d(4)(softbits+2 downto 1) ) +
									 unsigned(bm_corrbot(I)(K)(5)(softbits downto 1) );
		
		topmetric(I)(K)(softbits+3 downto 1) <= addtop_d(5)(softbits+3 downto 1);
		botmetric(I)(K)(softbits+3 downto 1) <= addbot_d(5)(softbits+3 downto 1);

	end if;
	end process clocking5;

	--topmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');
	--botmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');

end generate ifg_n5;


----*******************************
----***							***
----***			N = 6			***
----***							***
----*******************************


ifg_n6: IF (n_list(I)=6) and (m_list(I)=0) GENERATE


	clocking6 : Process (clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin
	if reset = '1' then
		topmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
		botmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
	elsif Rising_edge(clk) then

		addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
		addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
		addtop_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(4)(softbits downto 1) );
		addbot_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(4)(softbits downto 1) );
		addtop_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addtop_d(3)(softbits+1 downto 1) );
		addbot_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addbot_d(3)(softbits+1 downto 1) );				
		addtop_d(5)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(5)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(6)(softbits downto 1) );
		addbot_d(5)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(5)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(6)(softbits downto 1) );
		addtop_d(6)(softbits+3 downto 1) :=
									 unsigned('0' & addtop_d(4)(softbits+2 downto 1) ) +
									 unsigned(addtop_d(5)(softbits+1 downto 1) );
		addbot_d(6)(softbits+3 downto 1) :=
									 unsigned('0' & addbot_d(4)(softbits+2 downto 1) ) +
									 unsigned(addbot_d(5)(softbits+1 downto 1) );
		
		topmetric(I)(K)(softbits+3 downto 1) <= addtop_d(6)(softbits+3 downto 1);
		botmetric(I)(K)(softbits+3 downto 1) <= addbot_d(6)(softbits+3 downto 1);

	end if;
	end process clocking6;

  --topmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');
	--botmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');

end generate ifg_n6;


----*******************************
----***							***
----***			N = 7			***
----***							***
----*******************************


ifg_n7: IF (n_list(I)=7) and (m_list(I)=0) GENERATE


	clocking7 : Process (clk, reset)

		variable addtop_d, addbot_d : Vector_2D(n_list(I) downto 2);

	begin
	if reset = '1' then
		topmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
		botmetric(I)(K)(softbits+3 downto 1) <= (others => '0');
	elsif Rising_edge(clk) then

		addtop_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(2)(softbits downto 1) );
		addbot_d(2)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(1)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(2)(softbits downto 1) );
		addtop_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(4)(softbits downto 1) );
		addbot_d(3)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(3)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(4)(softbits downto 1) );
		addtop_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addtop_d(3)(softbits+1 downto 1) );
		addbot_d(4)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(2)(softbits+1 downto 1) ) +
									 unsigned('0' & addbot_d(3)(softbits+1 downto 1) );				
		addtop_d(5)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrtop(I)(K)(5)(softbits downto 1) ) +
										 unsigned('0' & bm_corrtop(I)(K)(6)(softbits downto 1) );
		addbot_d(5)(softbits+1 downto 1) :=
										 unsigned('0' & bm_corrbot(I)(K)(5)(softbits downto 1) ) +
										 unsigned('0' & bm_corrbot(I)(K)(6)(softbits downto 1) );
		addtop_d(6)(softbits+2 downto 1) :=
									 unsigned('0' & addtop_d(5)(softbits+1 downto 1) ) +
									 unsigned(bm_corrtop(I)(K)(7)(softbits downto 1) );
		addbot_d(6)(softbits+2 downto 1) :=
									 unsigned('0' & addbot_d(5)(softbits+1 downto 1) ) +
									 unsigned(bm_corrbot(I)(K)(7)(softbits downto 1) );
		addtop_d(7)(softbits+3 downto 1) :=
									 unsigned('0' & addtop_d(4)(softbits+2 downto 1) ) +
									 unsigned('0' & addtop_d(6)(softbits+2 downto 1) );
		addbot_d(7)(softbits+3 downto 1) :=
									 unsigned('0' & addbot_d(4)(softbits+2 downto 1) ) +
									 unsigned('0' & addbot_d(6)(softbits+2 downto 1) );				
		
		topmetric(I)(K)(softbits+3 downto 1) <= addtop_d(7)(softbits+3 downto 1);
		botmetric(I)(K)(softbits+3 downto 1) <= addbot_d(7)(softbits+3 downto 1);

	end if;
	end process clocking7;

  --topmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');
	--botmetric(I)(K)(BMG_MAX downto softbits+4) <= (others => '0');

end generate ifg_n7;

-- this is I think independent from n	: YES
-- but : is it independent from TCM mode? NO
  connect_L_max: if (l_list(I)=L_max) and (m_list(I)=0)  generate
	  bm_hypo_nil_top_premux(I)(K)(bm_size downto 1) <= topmetric(I)(K)(bm_size downto 1);
	  bm_hypo_nil_bot_premux(I)(K)(bm_size downto 1) <= botmetric(I)(K)(bm_size downto 1);
		bm_hypo_one_top_premux(I)(K)(bm_size downto 1) <= botmetric(I)(K)(bm_size downto 1);
		bm_hypo_one_bot_premux(I)(K)(bm_size downto 1) <= topmetric(I)(K)(bm_size downto 1);
	end generate connect_L_max;

  connect_L_nomax: if (l_list(I)<L_max) and (m_list(I)=0)  generate
	  bm_hypo_nil_top_premux(I)(K)(bm_size downto 1) <= topmetric(I)(K)(bm_size downto 1);
	  bm_hypo_nil_bot_premux(I)(K)(bm_size downto 1) <= topmetric(I)(K)(bm_size downto 1);
		bm_hypo_one_top_premux(I)(K)(bm_size downto 1) <= botmetric(I)(K)(bm_size downto 1);
		bm_hypo_one_bot_premux(I)(K)(bm_size downto 1) <= botmetric(I)(K)(bm_size downto 1);
	end generate connect_L_nomax;  


end generate fg_acs;
end generate fg_nco;




-----------------

--  I HAVE TO DO A MUX OF 4 BRANCH METRIC AND DO THE CONNECTION OF THE 2 VALUES 
-- TO THESE 4 BM BEFORE WHEN I NOW IF IT IS TCM OR VITERBI OR IF L_MAX > L


nff_mux : if ncodes >= 2 generate  -- MUX NON terminated in FF

met_mux_nff : Process(sel_code, bm_hypo_nil_top_premux, bm_hypo_nil_bot_premux,
											bm_hypo_one_top_premux, bm_hypo_one_bot_premux)

	--variable bm_hypo_nil_top_muxing, bm_hypo_nil_bot_muxing :	Vector_3D(0 to ncodes);
	--variable bm_hypo_one_top_muxing, bm_hypo_one_bot_muxing :	Vector_3D(0 to ncodes);
	variable bm_hypo_nil_top_muxing, bm_hypo_nil_bot_muxing :	cube_ac_bm(0 to ncodes);
	variable bm_hypo_one_top_muxing, bm_hypo_one_bot_muxing :	cube_ac_bm(0 to ncodes);
	variable tmp_and_sel : Std_Logic_Vector(sel_code'HIGH downto 0);

begin
  acs_loop: for K in 1 to ACS_units loop
    tmp_and_sel(0) := '1';
    bm_hypo_nil_top_muxing(0)(K)(bm_size downto 1) := (others => '0');
    bm_hypo_nil_bot_muxing(0)(K)(bm_size downto 1) := (others => '0');
		bm_hypo_one_top_muxing(0)(K)(bm_size downto 1) := (others => '0');
    bm_hypo_one_bot_muxing(0)(K)(bm_size downto 1) := (others => '0');

    or_loop: For I in 1 to ncodes loop
    	and_loop: For J in 1 to sel_code'HIGH loop
    		if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
    			tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code(J);
    		else  -- bit J of I-1 is 1
    			tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code(J);
    		end if;
    	end loop and_loop;
    	bm_hypo_nil_top_muxing(I)(K)(bm_size downto 1) :=
    	bm_hypo_nil_top_muxing(I-1)(K)(bm_size downto 1) or (
    	bm_hypo_nil_top_premux(I)(K)(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code'HIGH)));
    	bm_hypo_nil_bot_muxing(I)(K)(bm_size downto 1) :=
    	bm_hypo_nil_bot_muxing(I-1)(K)(bm_size downto 1) or (
    	bm_hypo_nil_bot_premux(I)(K)(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code'HIGH)));
			bm_hypo_one_top_muxing(I)(K)(bm_size downto 1) :=
    	bm_hypo_one_top_muxing(I-1)(K)(bm_size downto 1) or (
    	bm_hypo_one_top_premux(I)(K)(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code'HIGH)));
    	bm_hypo_one_bot_muxing(I)(K)(bm_size downto 1) :=
    	bm_hypo_one_bot_muxing(I-1)(K)(bm_size downto 1) or (
    	bm_hypo_one_bot_premux(I)(K)(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code'HIGH)));
    end loop or_loop;
  end loop acs_loop;	

  bm_hypo_nil_top_d <= bm_hypo_nil_top_muxing(ncodes);
  bm_hypo_nil_bot_d <= bm_hypo_nil_bot_muxing(ncodes);
	bm_hypo_one_top_d <= bm_hypo_one_top_muxing(ncodes);
	bm_hypo_one_bot_d <= bm_hypo_one_bot_muxing(ncodes);
end process met_mux_nff;

-- (ACS_units downto 1)(bm_size downto 1) doesn't work with type Vector_2D being defined
fg_acs2 : for K in 1 to ACS_units generate
	bm_hypo_nil_top(K)(bm_size downto 1) <= bm_hypo_nil_top_d(K)(bm_size downto 1);
	bm_hypo_nil_bot(K)(bm_size downto 1) <= bm_hypo_nil_bot_d(K)(bm_size downto 1);
	bm_hypo_one_top(K)(bm_size downto 1) <= bm_hypo_one_top_d(K)(bm_size downto 1);
	bm_hypo_one_bot(K)(bm_size downto 1) <= bm_hypo_one_bot_d(K)(bm_size downto 1);
end generate fg_acs2;

end generate nff_mux;

no_mux : if ncodes = 1 generate
	fg_acs3 : for K in 1 to ACS_units generate
        bm_hypo_nil_top(K)(32 downto bm_size+1) <= ( others =>'0');
        bm_hypo_nil_bot(K)(32 downto bm_size+1) <= ( others =>'0');
        bm_hypo_one_top(K)(32 downto bm_size+1) <= ( others =>'0');
        bm_hypo_one_bot(K)(32 downto bm_size+1) <= ( others =>'0');
        
		bm_hypo_nil_top(K)(bm_size downto 1) <= bm_hypo_nil_top_premux(1)(K)(bm_size downto 1);
		bm_hypo_nil_bot(K)(bm_size downto 1) <= bm_hypo_nil_bot_premux(1)(K)(bm_size downto 1);
		bm_hypo_one_top(K)(bm_size downto 1) <= bm_hypo_one_top_premux(1)(K)(bm_size downto 1);
		bm_hypo_one_bot(K)(bm_size downto 1) <= bm_hypo_one_bot_premux(1)(K)(bm_size downto 1);
	end generate fg_acs3;
end generate no_mux;

------------------

-- the doubling of the metrics for viterbi mode,maximal L ought to be done here
-- 4 branch metrics ought to come out if the metric unit
--bmtoptop => mettop_mux_q,	bmtopbot => metbot_mux_q,
--bmbottop => metbot_mux_q,	bmbotbot => mettop_mux_q,
-- new names
--bmtoptop -> bm_hypo_nil_top, bmtopbot -> bm_hypo_nil_bot,
--bmbottop -> bm_hypo_one_top, bmbotbot -> bm_hypo_one_bot,


-- Readability

--fg_read: For K in 1 to ACS_units generate
--if_Read1 : if BMG_MAX > softbits+1 generate

-- This has to be improved!! only for n=2 at the moment
--	hypo_nil_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	hypo_one_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--  writeadd_odd(I)(BMG_MAX downto seq_width+1) <= (others => '0');
--  writeadd_even(I)(BMG_MAX downto seq_width+1) <= (others => '0');
--	data_odd_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	data_even_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');

--end generate if_read1;

--end generate fg_read;


	
end architecture rtl;	
