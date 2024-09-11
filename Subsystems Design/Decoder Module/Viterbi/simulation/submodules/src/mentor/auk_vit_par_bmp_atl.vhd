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
-- $RCSfile: auk_vit_par_bmp_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/Parallel/atlantic/auk_vit_par_bmp_atl_arc_rtl.vhd,v $
--
-- $Revision: #4 $
-- $Date: 2013/03/04 $
-- Check in by 	 	 : $Author: shettiar $
-- Author      :  Alejandro Diaz-Manero
--
-- Project      :  Viterbi
--
-- Description	:  
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


Entity auk_vit_par_bmp_atl is
	generic (
		n 					: STRING  := "2_2_2_3";
		L 					: STRING  := "5_5_6_6";
		modes				: STRING  := "V_V_V_V";
		ncodes : NATURAL := 4;
		n_max : NATURAL := 3;
		log2_n_max : natural := 2;
		L_max : NATURAL := 6;
	  softbits : NATURAL := 4;
		bmgwide : NATURAL := 12;
		opt_par : STRING := "None";  -- none, continuous or block
		best_state_finder : STRING := "used";
		BER : STRING := "used";
		node_sync : STRING := "unused";
--		config : STRING := "std_no_max_info";
  -- possible others : std_no_max_info, ptcm_no_max_info, std_max_info and ptcm_max_info
		ga : STRING := "19_23_43_39";
		gb : STRING := "29_31_61_43";
		gc : STRING := "0_0_0_61";
		gd : STRING := "0_0_0_0";
		ge : STRING := "0_0_0_0";
		gf : STRING := "0_0_0_0";
		gg : STRING := "0_0_0_0"
		);		
	port (
		clk : in std_logic; 
		reset : in Std_Logic;
		enable : in Std_Logic;

		ena_master_sink : out Std_Logic; 
		val_sink : in Std_Logic; 
		sop_sink : in Std_Logic; 
		eop_sink : in Std_Logic; 
		allow_ena_assert_from_trb : in Std_Logic;

		state_node_sync : in Std_Logic_Vector(log2_n_max downto 1);
 		sel_code : in Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
		tr_init_state : in Std_Logic_Vector(L_max-1 downto 1);
		data_for_BER : out Std_Logic_Vector(2*n_max downto 1);
		rr : in Std_Logic_Vector(n_max*softbits downto 1);
		eras_sym : in Std_Logic_Vector(n_max downto 1);
		-- Alex 24-01-2007  rrff contains rr and eras_sym
		rrff : in std_logic_matrix(n_max downto 1, softbits+1 downto 1);
		survready, baddready : out Std_Logic;
		normalizations : out Std_Logic_Vector(8 downto 1);
 		survive : out Std_Logic_Vector(2**(L_max-1) downto 1);
		bestadd : out Std_Logic_Vector(L_max-1 downto 1);
		bestmet : out Std_Logic_Vector(bmgwide downto 1);
		sink_eop_del : out Std_Logic
	  );
end entity auk_vit_par_bmp_atl;

Architecture rtl of auk_vit_par_bmp_atl is

Constant maxstates  : NATURAL := 2**(L_max-1);
Constant halfstates : NATURAL := 2**(L_max-2);
Constant sel_code_size : NATURAL := LOG2_ceil_avoid_one(ncodes);
Constant ms_zero_init : Std_Logic_Vector(bmgwide downto 1) := --natural_2_m(arg => 2**(bmgwide-2), size => bmgwide);
             CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**(bmgwide-2), SIZE => bmgwide), SIZE => bmgwide);

Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant l_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);
Constant m_list : NATURAL_ARRAY(1 to ncodes) := Get_modes_list(modes => modes, ncodes => ncodes);
Constant n_max_codes : NATURAL := Get_n_max(n => n);
Constant L_min       : NATURAL := Get_n_min(n => l_list);
Constant L_diff      : NATURAL := L_max-L_min;
constant bm_size : NATURAL := softbits+LOG2_ceil_table(n_max_codes);
Constant j_init : NATURAL := j_init_function(opt_par);


constant tree_arch : NATURAL_ARRAY(0 to L_max-1) := Build_tree_arch(maxstates); -- (L=5 , 16, 8, 4, 2, 1)

Constant bin_tbl_max : NATURAL := Get_max_of_three(n_max, ncodes, 0);
Constant binary_table : Vector_2D(0 to 2**bin_tbl_max-1) := Build_binary_table(2**bin_tbl_max);
Constant binary_table_tr : Vector_2D(0 to maxstates-1) := Build_binary_table(maxstates);

Constant trellis_hypo_top : NATURAL_2D_ARRAY(ncodes downto 1, maxstates downto 1) := 
         Trellis_constructor_ncodes(n => n, L => L, ncodes => ncodes, n_max => n_max_codes, L_max => L_max, hypobit => '0',
                                    ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg);

Constant trellis_hypo_bot : NATURAL_2D_ARRAY(ncodes downto 1, maxstates downto 1) := 
         Trellis_constructor_ncodes(n => n, L => L, ncodes => ncodes, n_max => n_max_codes, L_max => L_max, hypobit => '1',
                                    ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg);


Constant n_max_tcm : NATURAL := Get_n_max_modes(n => n, m => modes);
Constant node_synch_ctrl_matrix : array_of_pla_tables_t(n_max_tcm downto 1) := 
    		 gen_node_sync_mux_ctrl(n => n, m => modes, n_max => n_max_tcm);

Subtype vector_ms is Std_Logic_Vector(maxstates downto 1);
Subtype vector_bm is Std_Logic_Vector(bm_size  downto 1);
Subtype vector_bm1 is Std_Logic_Vector(bm_size+1  downto 1);
Subtype vector_sm is Std_Logic_Vector(bmgwide  downto 1);
Subtype vector_lm is Std_Logic_Vector(L_max-1  downto 1);
Subtype vector_se is Std_Logic_Vector(sel_code_size downto 1);
Subtype vector_so is Std_Logic_Vector(softbits downto 1);
Subtype vector_ld is Std_Logic_Vector(L_diff  downto 0);


type matrix_ms is array(NATURAL RANGE <>) of vector_ms;
type matrix_sm is array(NATURAL RANGE <>) of vector_sm;
type matrix_lm is array(NATURAL RANGE <>) of vector_lm;
type matrix_bm is array(NATURAL RANGE <>) of vector_bm;
type matrix_bm1 is array(NATURAL RANGE <>) of vector_bm1;
type matrix_se is array(NATURAL RANGE <>) of vector_se;
type matrix_so is array(NATURAL RANGE <>) of vector_so;
type matrix_ld is array(NATURAL RANGE <>) of vector_ld;

Subtype matrix_bm_hs         is matrix_bm(halfstates  downto 1);
Subtype matrix_bm_ms         is matrix_bm(maxstates   downto 1);
Subtype matrix_sm_hs         is matrix_sm(halfstates  downto 1);
Subtype matrix_lm_ms         is matrix_lm(maxstates   downto 1);
Subtype matrix_sm_ms  			 is matrix_sm(maxstates   downto 1);
Subtype matrix_so_nc     		 is matrix_so(2**n_max_codes  downto 1);
Subtype matrix_bm_nc         is matrix_bm(2**n_max_codes  downto 1);

Type cube_bm_hs   is array(NATURAL RANGE <>) of matrix_bm_hs;
Type cube_bm_ms   is array(NATURAL RANGE <>) of matrix_bm_ms;
Type cube_sm_hs   is array(NATURAL RANGE <>) of matrix_sm_hs;
Type cube_bm_nc   is array(NATURAL RANGE <>) of matrix_bm_nc;
Type cube_lm_ms 	is ARRAY(NATURAL RANGE<>) of matrix_lm_ms;
Type cube_sm_ms 	is ARRAY(NATURAL RANGE<>) of matrix_sm_ms;
Type cube_so_nc 	is ARRAY(NATURAL RANGE<>) of matrix_so_nc;

Subtype cube_so_nc_nm is cube_so_nc(n_max_codes downto 1);

Type cube_4D is array(NATURAL RANGE <>) of cube_so_nc_nm;



Constant bm_init_matrix : Std_Logic_3D_CUBE(ncodes downto 1, halfstates downto 1, bmgwide downto 1) := 
			 init_state_metric_constructor(L_max => L_max, L => L, ncodes => ncodes, bmgwide => bmgwide);

Signal corrtop, corrbot : cube_4D(ncodes downto 1);

signal rrff_d, rrff_q, rrffa_d, rrffa_q : matrix_bm(n_max_tcm downto 1);

Signal addtop_q, addbot_q, addtop_d, addbot_d : cube_bm_nc(ncodes downto 1);
Signal bmtop_mux_q, bmbot_mux_q : matrix_bm(maxstates downto 1);
Signal bmtop_mux_d, bmbot_mux_d : matrix_bm(maxstates downto 1);
Signal bmdiff_mux_q : matrix_bm1(maxstates downto 1);
Signal bmsnode_ini : matrix_sm(halfstates downto 1);

Signal bms_q : matrix_sm(maxstates downto 1);

signal bmgsel_d, bmgsel_q : cube_sm_ms(L_max-1 downto 0);
signal bmgaddsel_d, bmgaddsel_q : cube_lm_ms(L_max-1 downto 0);

signal sel_tree : matrix_ms(L_max-1 downto 0);

Signal survff_q, over_and : Std_Logic_Vector(maxstates downto 1);
Signal overhold_and_q : Std_Logic_Vector(3 downto 1);

signal rxbits, rxzip : Std_Logic_Vector(n_max downto 1);
Signal val_sink_del : Std_Logic_Vector(L_max+3 downto 1);
Signal eop_sink_del : Std_Logic_Vector(L_max+3 downto 1);
signal sel_code_del : matrix_se(5 downto 1);
Signal init_mem, init_launch : Std_Logic;
Signal normalizations_int : Std_Logic_Vector(8 downto 1);

Signal allow_ena_assert, init_launch_q, intr_eop_gap : Std_Logic;
signal gap_eop_state, gap_eop_state_del, val_sink_extra : std_logic;

Signal tr_init_state_mux : std_logic_vector(L_max-1 downto 1);
Signal L_diffs_mux : std_logic_vector(L_diff downto 0);
signal demux_tr_init, clear_bmp : std_logic_vector(maxstates downto 1);
Signal tr_init_state_mux_in : matrix_lm(1 to ncodes);
Signal L_diffs_mux_in : matrix_ld(1 to ncodes);
Signal eop_sink_3_connect, eop_sink_4_connect : std_logic;
Signal init_mem_connect, init_launch_connect : std_logic;

begin




ifg_cnt: if opt_par="Continuous"   generate
	clear_bmp <= (others => '0');
	baddready <= '0';
	sink_eop_del <= '0';
	gap_eop_state	 <= '0';
	eop_sink_3_connect <= '0';
	eop_sink_4_connect <= '0';
	allow_ena_assert <= allow_ena_assert_from_trb;
	init_mem_connect <= '1';
	init_launch_connect <= '0';
end generate ifg_cnt;

ifg_not_cnt: if opt_par/="Continuous"   generate
	baddready <= val_sink_del(L_max+3);
	-- Alex 13-2-2007  the actual lenght for sink_eop_del depends on which architecture 
	-- AND if it has best state finder or not, if there is no best state finder then 
	-- it is just like survready.
	--no_best_sf: if best_state_finder="unused" generate
	block_case: if opt_par="Block"   generate
		sink_eop_del <= eop_sink_del(4);  -- like survready
	end generate block_case;
	--best_sf: if best_state_finder="used" generate
	standard_case: if opt_par="None"   generate
	-- Alex 13-02-2007 this is going to be tricky in new parallel block
	-- I may have to consider telling my manager to disable this option
	-- hence no best state finder in parallel block
		sink_eop_del <= eop_sink_del(L_max+3);  
	end generate standard_case;
	eop_sink_3_connect <= eop_sink_del(3);
	eop_sink_4_connect <= eop_sink_del(4);
	-- Alex 19-01-2007  I have to check this logic connection for block
	allow_ena_assert <= allow_ena_assert_from_trb and intr_eop_gap;
	init_mem_connect <= init_mem;
	init_launch_connect <= init_launch;
end generate ifg_not_cnt;

ifg7: if ncodes=1 and opt_par/="Continuous"   generate
	intr_eop_gap <= '1';
	for_cl2: for I in 1 to maxstates generate
		clear_bmp(I) <= eop_sink_del(3);
	end generate for_cl2;
                                -- gap_eop_state	 <= '1'; --'1' or '0'??
end generate ifg7;

ifg6: if ncodes>1 and opt_par/="Continuous" generate

	for_cl: for I in 1 to maxstates generate
		clear_bmp(I) <= (eop_sink_del(3) and not init_mem) or (eop_sink_del(4) and not eop_sink_del(5) and init_mem) or 
										(eop_sink_del(3) and not eop_sink_del(4) and init_mem and demux_tr_init(I));
	end generate for_cl; 

	intr_eop_gap <= (not eop_sink) or gap_eop_state or gap_eop_state_del;
	clk_gap_state: Process (clk, reset)
	begin
		if reset='1' then
			gap_eop_state	 <= '0';
			gap_eop_state_del	 <= '0';
		elsif Rising_edge(clk) then
			if eop_sink='1' and gap_eop_state='0' and gap_eop_state_del='0' then
			  gap_eop_state <= '1';
			elsif gap_eop_state='1' then 
				gap_eop_state <= '0';
			end if;
			if gap_eop_state='1' and gap_eop_state_del='0' then
			  gap_eop_state_del <= '1';
			elsif gap_eop_state_del='1' and eop_sink='0' then
				gap_eop_state_del <= '0';
			end if;
		end if;
	end process clk_gap_state;
	
	forg_in : for I in 1 to ncodes generate
		ifg_0: if L_max-L_list(I)=0 generate
			tr_init_state_mux_in(I)(L_max-1 downto 1) <= tr_init_state;
			L_diffs_mux_in(I) <= (others => '0');
		end generate ifg_0;
		ifg_1: if L_max-L_list(I)=1 generate
			tr_init_state_mux_in(I)(L_max-1 downto 2) <= tr_init_state(L_max-2 downto 1);
			tr_init_state_mux_in(I)(1) <= '-'; -- don't care
			L_diffs_mux_in(I)(0) <= '1';
			L_diffs_mux_in(I)(L_diff downto 1) <= (others => '0');
		end generate ifg_1;
		ifg_2: if L_max-L_list(I)=2 generate
			tr_init_state_mux_in(I)(L_max-1 downto 3) <= tr_init_state(L_max-3 downto 1);
			tr_init_state_mux_in(I)(2 downto 1) <= "--"; -- don't care
			L_diffs_mux_in(I)(1 downto 0) <= "11";
			L_diffs_mux_in(I)(L_diff downto 2) <= (others => '0');
		end generate ifg_2;
		ifg_3: if L_max-L_list(I)=3 generate
			tr_init_state_mux_in(I)(L_max-1 downto 4) <= tr_init_state(L_max-4 downto 1);
			tr_init_state_mux_in(I)(3 downto 1) <= "---"; -- don't care
			L_diffs_mux_in(I)(2 downto 0) <= "111";
			L_diffs_mux_in(I)(L_diff downto 3) <= (others => '0');
		end generate ifg_3;
		ifg_4: if L_max-L_list(I)=4 generate
			tr_init_state_mux_in(I)(L_max-1 downto 5) <= tr_init_state(L_max-5 downto 1);
			tr_init_state_mux_in(I)(4 downto 1) <= "----"; -- don't care
			L_diffs_mux_in(I)(3 downto 0) <= "1111";
			L_diffs_mux_in(I)(L_diff downto 4) <= (others => '0');
		end generate ifg_4;
		ifg_5: if L_max-L_list(I)=5 generate
			tr_init_state_mux_in(I)(L_max-1 downto 6) <= tr_init_state(L_max-6 downto 1);
			tr_init_state_mux_in(I)(5 downto 1) <= "-----"; -- don't care
			L_diffs_mux_in(I)(4 downto 0) <= "11111";
			L_diffs_mux_in(I)(L_diff downto 5) <= (others => '0');
		end generate ifg_5;
		ifg_6: if L_max-L_list(I)=6 generate
			tr_init_state_mux_in(I)(L_max-1 downto 7) <= tr_init_state(L_max-7 downto 1);
			tr_init_state_mux_in(I)(6 downto 1) <= "------"; -- don't care
			L_diffs_mux_in(I)(5 downto 0) <= "111111";
			L_diffs_mux_in(I)(L_diff downto 6) <= (others => '0');
		end generate ifg_6;
	end generate forg_in;
	
	-- Logic for demuxing tr_init_state and sel_code
	ncodes_mux_3 : Process(tr_init_state_mux_in, sel_code_del(1), L_diffs_mux_in)
	
		variable mux_temp_p :	matrix_lm(0 to ncodes);
		variable mux_temp_b :	matrix_ld(0 to ncodes);
		variable tmp_and_sel : Std_Logic_Vector(sel_code_del(1)'HIGH downto 0);
		variable mux_addition : std_logic_vector(L_max-1 downto 1);
		variable mux_add_b : std_logic_vector(L_diff downto 0);
	
	begin
		
		tmp_and_sel(0) := '1';
		mux_temp_p(0) := (others => '0');
		mux_temp_b(0) := (others => '0');
		or_loop: For I in 1 to ncodes loop
			and_loop: For J in 1 to sel_code_del(1)'HIGH loop
				if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
					tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_del(1)(J);
				else  -- bit J of I-1 is 1
					tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_del(1)(J);
				end if;
			end loop and_loop;
			-- moving to MSB 
			mux_addition := 
				tr_init_state_mux_in(I)(L_max-1 downto 1) and (L_max-1 downto 1 => tmp_and_sel(sel_code_del(1)'HIGH));
			mux_add_b := 
				L_diffs_mux_in(I)(L_diff downto 0) and (L_diff downto 0 => tmp_and_sel(sel_code_del(1)'HIGH));
			mux_temp_p(I) := mux_temp_p(I-1) or mux_addition;
			mux_temp_b(I) := mux_temp_b(I-1) or mux_add_b;
		end loop or_loop;
		tr_init_state_mux <= mux_temp_p(ncodes);
		L_diffs_mux <= mux_temp_b(ncodes);
			
	end process ncodes_mux_3;
	
	
	ncodes_demux : Process(tr_init_state_mux, L_diffs_mux)
	
		variable demux_tr_init_var : std_logic_vector(maxstates downto 1);
		variable tmp_and_sel : Std_Logic_Vector(L_max-1 downto 0);
	
	begin
	  floop: for I in 1 to maxstates loop
			tmp_and_sel(0) := '1';
			and_loop: For J in 1 to L_max-1 loop
			--is this right or could I just just the and function?
				if J-1>L_diff then
					if binary_table_tr(I-1)(J)='0' then --bit J of I-1 is 0
						tmp_and_sel(J) := tmp_and_sel(J-1) and not tr_init_state_mux(J);
					else  --bit J of I-1 is 1
						tmp_and_sel(J) := tmp_and_sel(J-1) and tr_init_state_mux(J);
					end if;
				else
					if L_diffs_mux(J-1)='1' then
						tmp_and_sel(J) := tmp_and_sel(J-1);
					else
						if binary_table_tr(I-1)(J)='0' then 
							tmp_and_sel(J) := tmp_and_sel(J-1) and not tr_init_state_mux(J);
						else  
							tmp_and_sel(J) := tmp_and_sel(J-1) and tr_init_state_mux(J);
						end if;
					end if;
				end if;
			end loop and_loop;
			demux_tr_init_var(I) := not tmp_and_sel(L_max-1);
		end loop floop;
		demux_tr_init <= demux_tr_init_var;
		
	end process ncodes_demux;
	
end generate ifg6;
	
-- latching atlantic ports

clk_atl: Process (clk, reset)
	begin
		if reset='1' then
			ena_master_sink	 <= '0';
			eop_sink_del(1) <= '0';
			val_sink_del(1) <= '0';
			sel_code_del(1) <= (others => '0');
			init_mem <= '0';
			val_sink_extra <= '0';
		elsif Rising_edge(clk) then
		  -- Alex 23-01-2007  now I want this FF removed . I want a direct connection from 
			-- allow ena_assert to ena_sink_master
			ena_master_sink	 <= allow_ena_assert;
			-- Alex 18-01-2007  with continuous optimization gap_eop_state=0 always
			-- so val_sink_extra=0 always
			if val_sink='1' and gap_eop_state='1' then
				val_sink_extra <= '1';
			else
				val_sink_extra <= '0';
			end if;
			if gap_eop_state='1' then
				val_sink_del(1) <= '0';
			elsif val_sink_extra='1' then
				val_sink_del(1) <= '1';
			else
				val_sink_del(1) <= val_sink;
			end if;
			--  24-01-2007  this entry point dissappears for continuous
			if val_sink='1' then 
				eop_sink_del(1) <= eop_sink;
			end if;
			if val_sink_del(1)='1' then
			  sel_code_del(1) <= sel_code;
			end if;
			-- Alex 18-01-2007  always zero here if continuous optimization
			init_mem <= clear_bmp(1) or init_mem;
		end if;
		
end process clk_atl;

survready <= val_sink_del(4);

-- I have to work here on adding reset-init logic
-- logic to set for 1 cc sink_eop_del(3) 
-- and val_sink_del(3) without propagation
-- at the start of init in state_trb

init_launch <= sop_sink and not init_mem and not init_launch_q;

fg2: for i in 2 to l_max+3 generate

--ifg34: if i/=3 and i/=4 and i/=2 generate
ifg34: if i/=3 and i/=4 generate
	del_eop: Process (clk, reset)
		begin
			if reset='1' then
				eop_sink_del(I) <= '0';
				val_sink_del(I) <= '0';
			elsif Rising_edge(clk) then
				val_sink_del(I) <= val_sink_del(I-1);
				if val_sink_del(I-1)='1' then
				  eop_sink_del(I) <= eop_sink_del(I-1);
				end if;
			end if;
	end process del_eop;
end generate ifg34;

ifg33: if i=3 generate
	del_eop: Process (clk, reset)
		begin
			if reset='1' then
				eop_sink_del(I) <= '0';
				val_sink_del(I) <= '0';
				init_launch_q <= '0';
			elsif Rising_edge(clk) then
				init_launch_q <= init_launch;
				-- disconnect init_launch for continuous
				val_sink_del(I) <= val_sink_del(I-1) or init_launch_connect;
				if val_sink_del(I-1)='1' or init_launch='1' then
				--if init_launch='1' then
				  eop_sink_del(I) <= eop_sink_del(I-1) or init_launch;
				end if;
			end if;
	end process del_eop;
end generate ifg33;

ifg44: if i=4 generate
	del_eop: Process (clk, reset)
		begin
			if reset='1' then
				eop_sink_del(I) <= '0';
				val_sink_del(I) <= '0';
			elsif Rising_edge(clk) then
			 --  This connection here I have to set init_mem to 1 for continuous
				val_sink_del(I) <= val_sink_del(I-1) and init_mem_connect;
				if val_sink_del(I-1)='1' then
				  eop_sink_del(I) <= eop_sink_del(I-1) and init_mem;
				end if;
			end if;
	end process del_eop;
end generate ifg44;

end generate fg2;


fg3: FOR k IN 1 TO n_max GENERATE
	
	ifg3b: if node_sync="unused" generate
		fg3b: for J in 1 to softbits+1 generate			
			rrffa_d(k)(J) <= rrff(k, J);
		end generate fg3b;
	end generate ifg3b;
	
	-- Duong : SPR 262951 & 269659
	-- create rrff_q to use inside node_sync_logic
	-- if node_sync = "used", implement a node_sync_logic to syncronize the input
	-- rrff will be go through that logic, and be rotation to form rrffa_d.
	-- if node_sync = "unused", connect rrffa_d to rrff directly.
	ifg3c: if node_sync="used" generate
		fg3c: for J in 1 to softbits+1 generate	
			rrff_q(k)(J) <= rrff(k, J);			
		end generate fg3c;
	end generate ifg3c;
	
END GENERATE fg3;




rrff_reg : Process (clk, reset)
begin
if reset = '1' then
	--rrff_q <= (others => (others => '0'));
	rrffa_q <= (others => (others => '0'));
elsif Rising_edge(clk) then
	--  23-01-2007  There is no shunt buffer here
	--  val_sink ought to be the united enable in the continuous optimization
	-- take rrff_q out of bmp
	--if val_sink='1' then
	--	rrff_q <= rrff_d;
	--end if;
	if val_sink_del(1)='1' then
		rrffa_q <= rrffa_d;
	end if;
end if;
end process rrff_reg;


node_sync_logic: if node_sync="used" generate

  g2: for K in 1 to n_max generate

-- do I need a different node_synch_ctrl_matrix with multicode?

-- Alex 24-01-2007  I need to ammend rrff_q for nodesync case

    pla_2_mux: process(state_node_sync, rrff_q)  -- I'll fit the mux as well
  			variable pla_mux_sel : Std_Logic_Vector(log2_n_max downto 1); -- Vector_2D(n_max downto 1);
  			variable rrff_mux_p :	Vector_2D(n_max downto 0);  -- Vector_3D_n_max(n_max downto 0);
  			variable tmp_and_sel : Std_Logic_Vector(log2_n_max downto 0);
    begin
			pla_table ( state_node_sync, pla_mux_sel, node_synch_ctrl_matrix(K));
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


-- If the quantification scheme changes to even 2^softbits levels 
--  then BER would have to stop considering 000 as erased symbol.
if_ber : if BER="used" generate

  fg2: for j in 1 to n_max generate
    rxbits(j) <= rrffa_q(j)(softbits);
		rxzip(j) <= not rrffa_q(j)(softbits+1);
  end generate fg2;

  data_for_BER(2*n_max downto n_max+1) <= rxzip;
  data_for_BER(n_max downto 1) <= rxbits;

end generate if_ber;

if_no_ber : if not(BER="used") generate
  data_for_BER <= (others => '0');
end generate if_no_ber;


multicode: for I in 1 to ncodes generate


	ifg_TCM: IF m_list(I)=1 GENERATE -- n_list(I) is supposed to be 2

		gen_j_n2: For j in 1 to 2**n_list(I) generate  --  n= 2
			addtop_d(I)(j)(softbits downto 1) <= rrffa_q(j)(softbits downto 1) and 
										(softbits downto 1 => not rrffa_q(j)(softbits+1));
			addtop_d(I)(j)(bm_size downto softbits+1) <= (others => '0');
		end generate gen_j_n2;
	  
	end generate ifg_TCM;

	ifg_corr: IF m_list(I)=0 GENERATE 

		ifsoft: if softbits>1 generate

			corr_proc : Process (rrffa_q)

			begin
			j_n: For j in 1 to 2**n_list(I) loop  --  n= 2
				k_n: For K in 1 to n_list(I) loop   -- n= 2
					if(binary_table(j-1)(K)='1') then
						corrtop(I)(K)(j)(softbits-1 downto 1) <= not (rrffa_q(K)(softbits-1 downto 1) or (softbits-1 downto 1 => rrffa_q(K)(softbits+1)));
						corrtop(I)(K)(j)(softbits) <= rrffa_q(K)(softbits) and not rrffa_q(K)(softbits+1);
					else
						corrtop(I)(K)(j)(softbits-1 downto 1) <= rrffa_q(K)(softbits-1 downto 1) and (softbits-1 downto 1 => not rrffa_q(K)(softbits+1));
						corrtop(I)(K)(j)(softbits) <= not (rrffa_q(K)(softbits) or rrffa_q(K)(softbits+1));
					end if;
				end loop k_n;
			end loop j_n;

			end process corr_proc;

		end generate ifsoft;

		ifsoft_one: if softbits=1 generate

			corr_proc : Process (rrffa_q)

			begin
			j_n: For j in 1 to 2**n_list(I) loop  --  n= 2
				k_n: For K in 1 to n_list(I) loop   -- n= 2
					if(binary_table(j-1)(K)='1') then
						corrtop(I)(K)(j)(softbits) <= rrffa_q(K)(softbits) and not rrffa_q(K)(softbits+1);
					else
						corrtop(I)(K)(j)(softbits) <= not (rrffa_q(K)(softbits) or rrffa_q(K)(softbits+1));
					end if;
				end loop k_n;
			end loop j_n;

			end process corr_proc;

		end generate ifsoft_one;
	  
	end generate ifg_corr;


	--*************
	--*** n = 2 ***
	--*************

	ifg_n2: IF n_list(I)=2 and m_list(I)=0 GENERATE

		ifg_bm_soft2: if bm_size > softbits+1 generate
			bm_cal_n2 : Process (corrtop, corrbot)
	
				variable addtop : cube_bm_nc(n_list(I) downto 1);
	
			begin
			clock_j_n2: For j in 1 to 2**n_list(I) loop  --  n= 2
	
				addtop(2)(j)(softbits+1 downto 1) :=
											 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
											 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
	
				addtop_d(I)(j)(softbits+1 downto 1) <= addtop(2)(j)(softbits+1 downto 1);
				addtop_d(I)(j)(bm_size downto softbits+2) <= (others => '0');
			end loop clock_j_n2;
			end process bm_cal_n2;
		end generate ifg_bm_soft2;
		
		ifg_not_bm_soft2: if not (bm_size > softbits+1) generate
			bm_cal_n2 : Process (corrtop, corrbot)
	
				variable addtop : cube_bm_nc(n_list(I) downto 1);
	
			begin
			clock_j_n2: For j in 1 to 2**n_list(I) loop  --  n= 2
	
				addtop(2)(j)(softbits+1 downto 1) :=
											 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
											 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
	
				addtop_d(I)(j)(softbits+1 downto 1) <= addtop(2)(j)(softbits+1 downto 1);
			end loop clock_j_n2;
			end process bm_cal_n2;
		end generate ifg_not_bm_soft2;
 
	end generate ifg_n2;

	--*************
	--*** n = 3 ***
	--*************

	ifg_n3: IF n_list(I)=3 and m_list(I)=0 GENERATE

		bm_cal_n3 : Process (corrtop, corrbot)

			--variable addtop, addbot : matrix_3D(3 downto 1);
			variable addtop, addbot : cube_bm_nc(3 downto 1);

		begin

		clock_j_n3: For j in 1 to 2**3 loop

			addtop(2)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );

			addtop(3)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(2)(j)(softbits+1 downto 1) ) +
										 unsigned(corrtop(I)(3)(j)(softbits   downto 1) );

			addtop_d(I)(j)(softbits+2 downto 1) <= addtop(3)(j)(softbits+2 downto 1);				

		end loop clock_j_n3;

		end process bm_cal_n3;

	end generate ifg_n3;

	--*************
	--*** n = 4 ***
	--*************

	ifg_n4: IF n_list(I)=4 and m_list(I)=0 GENERATE

		bm_cal_n4 : Process (corrtop, corrbot)

			variable addtop, addbot : cube_bm_nc(n_list(I) downto 1);

		begin

		clock_j_n4: For j in 1 to 2**n_list(I) loop

			addtop(2)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
				
			addtop(3)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(3)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(4)(j)(softbits downto 1) );

			addtop(4)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(2)(j)(softbits+1 downto 1) ) +
										 unsigned('0' & addtop(3)(j)(softbits+1 downto 1) );
			
			addtop_d(I)(j)(softbits+2 downto 1) <= addtop(4)(j)(softbits+2 downto 1);

		end loop clock_j_n4;

		end process bm_cal_n4;

	end generate ifg_n4;


   
	--*************
	--*** n = 5 ***
	--*************

	ifg_n5: IF n_list(I)=5 and m_list(I)=0 GENERATE

		bm_cal_n5 : Process (corrtop, corrbot)

			variable addtop, addbot : cube_bm_nc(n_list(I) downto 1);

		begin

		clock_j_n5: For j in 1 to 2**n_list(I) loop

			addtop(2)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
				
			addtop(3)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(3)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(4)(j)(softbits downto 1) );

			addtop(4)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(2)(j)(softbits+1 downto 1) ) +
										 unsigned('0' & addtop(3)(j)(softbits+1 downto 1) );
                                         
			addtop(5)(j)(softbits+3 downto 1) :=
										 unsigned('0' & addtop(4)(j)(softbits+2 downto 1) ) +
										 unsigned('0' & corrtop(I)(5)(j)(softbits downto 1) );
                                         
			addtop_d(I)(j)(softbits+3 downto 1) <= addtop(5)(j)(softbits+3 downto 1);

		end loop clock_j_n5;

		end process bm_cal_n5;

	end generate ifg_n5;
    
    
	--*************
	--*** n = 6 ***
	--*************

	ifg_n6: IF n_list(I)=6 and m_list(I)=0 GENERATE

		bm_cal_n6 : Process (corrtop, corrbot)

			variable addtop, addbot : cube_bm_nc(n_list(I) downto 1);

		begin

		clock_j_n6: For j in 1 to 2**n_list(I) loop

			addtop(2)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
				
			addtop(3)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(3)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(4)(j)(softbits downto 1) );

			addtop(4)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(2)(j)(softbits+1 downto 1) ) +
										 unsigned('0' & addtop(3)(j)(softbits+1 downto 1) );
                                         
			addtop(5)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(5)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(6)(j)(softbits downto 1) );
                                         
			addtop(6)(j)(softbits+3 downto 1) :=
										 unsigned('0' & addtop(4)(j)(softbits+2 downto 1) ) +
										 unsigned('0' & addtop(5)(j)(softbits+1 downto 1) );
                                         
			addtop_d(I)(j)(softbits+3 downto 1) <= addtop(6)(j)(softbits+3 downto 1);

		end loop clock_j_n6;

		end process bm_cal_n6;

	end generate ifg_n6;
    
    
	--*************
	--*** n = 7 ***
	--*************

	ifg_n7: IF n_list(I)=7 and m_list(I)=0 GENERATE

		bm_cal_n7 : Process (corrtop, corrbot)

			variable addtop, addbot : cube_bm_nc(n_list(I) downto 1);

		begin

		clock_j_n7: For j in 1 to 2**n_list(I) loop

			addtop(2)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(1)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(2)(j)(softbits downto 1) );
				
			addtop(3)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(3)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(4)(j)(softbits downto 1) );

			addtop(4)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(2)(j)(softbits+1 downto 1) ) +
										 unsigned('0' & addtop(3)(j)(softbits+1 downto 1) );
                                         
			addtop(5)(j)(softbits+1 downto 1) :=
										 unsigned('0' & corrtop(I)(5)(j)(softbits downto 1) ) +
										 unsigned('0' & corrtop(I)(6)(j)(softbits downto 1) );
                                         
			addtop(6)(j)(softbits+2 downto 1) :=
										 unsigned('0' & addtop(5)(j)(softbits+1 downto 1) ) +
										 unsigned('0' & corrtop(I)(7)(j)(softbits downto 1) );
                                         
			addtop(7)(j)(softbits+3 downto 1) :=
										 unsigned('0' & addtop(4)(j)(softbits+2 downto 1) ) +
										 unsigned('0' & addtop(6)(j)(softbits+2 downto 1) );
                                         
			addtop_d(I)(j)(softbits+3 downto 1) <= addtop(7)(j)(softbits+3 downto 1);

		end loop clock_j_n7;

		end process bm_cal_n7;

	end generate ifg_n7;

end generate multicode;


ifg_one_code: if ncodes=1 generate

	bm_latch: process(clk, reset)

	begin

    if reset='1' then
			addtop_q(1) <= (others => (others => '0'));
			bmdiff_mux_q <= (others => (others => '0'));
    elsif Rising_edge(clk) then
			if val_sink_del(2)='1' then
				for I in 1 to 2**n_max_codes loop
				 addtop_q(1)(I)(bm_size downto 1)	<= addtop_d(1)(I)(bm_size downto 1);
--             bmdiff_mux_q(I)(bm_size+1 downto 1) <= ('0' & addtop_d(1)(trellis_hypo_top(1,I))(bm_size downto 1)) - ('0' & addtop_d(1)(trellis_hypo_bot(1,I))(bm_size downto 1));
				end loop;
				for I in 1 to maxstates loop
--				 addtop_q(1)(I)(bm_size downto 1)	<= addtop_d(1)(I)(bm_size downto 1);
             bmdiff_mux_q(I)(bm_size+1 downto 1) <= ('0' & addtop_d(1)(trellis_hypo_top(1,I))(bm_size downto 1)) - ('0' & addtop_d(1)(trellis_hypo_bot(1,I))(bm_size downto 1));
				end loop;
			end if;
		end if;

	end process bm_latch;

-- notice now that branch metrics are in addtop only ... Addbot not needed or superfluous
-- it is the trellis_hypo which determines which branch metric to use in every branch of the 
-- trellis depending on the hypothesis. 
-- Addtop is just the set of possible branch metric values for all hypothesis.
-- notice as well that there is a multiple bm_top_mux_q branch metrics 
-- while in reality there are just 4 or 2^n  
-- need to come up with scheme to optimize that latching thus reducing the number of branch metrics
-- around

	arrange_bm: for K in 1 to maxstates generate

	  bmtop_mux_q(K)(bm_size downto 1) <= addtop_q(1)(trellis_hypo_top(1,K))(bm_size downto 1);
	  bmbot_mux_q(K)(bm_size downto 1) <= addtop_q(1)(trellis_hypo_bot(1,K))(bm_size downto 1);
		ifg_11 : if K=1 generate
			bmsnode_ini(K) <= ms_zero_init;
		end generate ifg_11;
		ifg_12 : if K>1 and K<=halfstates generate
			bmsnode_ini(K) <= (others => '0');
		end generate ifg_12;
	end generate arrange_bm;

end generate ifg_one_code;



ifg_ncodes: if ncodes > 1 generate

fg22: for i in 2 to 5 generate

	del_selcode: Process (clk, reset)
		begin
			if reset='1' then
				sel_code_del(I) <= (others => '0');
			elsif Rising_edge(clk) then
				if val_sink_del(I-1)='1' then
				  sel_code_del(I) <= sel_code_del(I-1);
				end if;
			end if;
			
	end process del_selcode;
end generate fg22;

  bm_mux: process(addtop_d, addbot_d, sel_code_del(1))

		variable bmtop_mux_op, bmbot_mux_op : cube_bm_ms(0 to ncodes);
		variable sel_code_var : Std_Logic_Vector(sel_code_size downto 1);
		variable tmp_and_sel : Std_Logic_Vector(sel_code_var'HIGH downto 0);

	begin

	  tmp_and_sel(0) := '1';
		sel_code_var := sel_code_del(1);
		arrange_bm: for K in 1 to maxstates loop
	  bmtop_mux_op(0)(K)(bm_size downto 1) := (others => '0');
	  bmbot_mux_op(0)(K)(bm_size downto 1) := (others => '0');

		  or_loop: For I in 1 to ncodes loop
		  	and_loop: For J in 1 to sel_code'HIGH loop
		  		if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
		  			tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_var(J);
		  		else  -- bit J of I-1 is 1
		  			tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_var(J);
		  		end if;
		  	end loop and_loop;
		  	bmtop_mux_op(I)(K)(bm_size downto 1) :=
		  	bmtop_mux_op(I-1)(K)(bm_size downto 1) or (
		  	addtop_d(I)(trellis_hypo_top(I,K))(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code_var'HIGH)));
		  	bmbot_mux_op(I)(K)(bm_size downto 1) :=
		  	bmbot_mux_op(I-1)(K)(bm_size downto 1) or (
		  	addtop_d(I)(trellis_hypo_bot(I,K))(bm_size downto 1) and (bm_size downto 1 => tmp_and_sel(sel_code_var'HIGH)));
		  end loop or_loop;
		end loop arrange_bm;

	  bmtop_mux_d <= bmtop_mux_op(ncodes);
	  bmbot_mux_d <= bmbot_mux_op(ncodes);
	
	end process bm_mux;

	bm_latch: process(clk, reset)

	begin

    if reset='1' then
			bmtop_mux_q <= (others => (others => '0'));
	  	bmbot_mux_q <= (others => (others => '0'));
		elsif Rising_edge(clk) then
			if val_sink_del(2)='1' then
		  	bmtop_mux_q <= bmtop_mux_d;
			  bmbot_mux_q <= bmbot_mux_d;
			end if;
		end if;

	end process bm_latch;


	-- Alex 18-01-2007  eop_sink_del(3), eop_sink_del(4) have to be 
	-- disconnected for continuous optimization, so it is init_mem
	-- I think this logic ought to be removed anyway for continuous optimization
	
  bm_init_mux: process(sel_code, eop_sink_3_connect, eop_sink_4_connect, init_mem)

		variable bm_mux_op : cube_sm_hs(0 to ncodes);
		variable sm_init_item : Std_Logic_Vector(bmgwide downto 1);
		variable tmp_and_sel : Std_Logic_Vector(sel_code'HIGH downto 0);

	begin

	  tmp_and_sel(0) := '1';
		arrange_bm: for K in 1 to halfstates loop
		  bm_mux_op(0)(K)(bmgwide downto 1) := (others => '0');

		  or_loop: For I in 1 to ncodes loop
		  	item_loop: for H in 1 to bmgwide loop
					sm_init_item(H) := bm_init_matrix(I, K, H);
				end loop item_loop;
		  	and_loop: For J in 1 to sel_code'HIGH loop
		  		if binary_table(I-1)(J)='0' then -- bit J of I-1 is 0
		  			tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code(J);
		  		else  -- bit J of I-1 is 1
		  			tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code(J);
		  		end if;
		  	end loop and_loop;
		  	bm_mux_op(I)(K)(bmgwide downto 1) :=
		  	bm_mux_op(I-1)(K)(bmgwide downto 1) or (
		  	sm_init_item(bmgwide downto 1) and (bmgwide downto 1 => tmp_and_sel(sel_code'HIGH) and 
				                not (eop_sink_3_connect and not eop_sink_4_connect and init_mem)));
		  end loop or_loop;
		end loop arrange_bm;

	  bmsnode_ini <= bm_mux_op(ncodes);
	
	end process bm_init_mux;

end generate ifg_ncodes;

--------

gbna: FOR j IN 1 TO maxstates generate

-- I need bms_q(1) initialized at asynch reset
ifg5_cnt : if j=1 and opt_par="Continuous" generate

     
	clocking : Process (clk, reset)

		variable bmsnode : Std_Logic_Vector(bmgwide downto 1);
		variable add_metric_top, add_metric_bot : Std_Logic_Vector(bmgwide downto 1);
		variable choosetop : Std_Logic;
		variable bmetrictop, bmetricbot, smetrictop, smetricbot : Std_Logic_Vector(bmgwide downto 1);
		variable cmp : Std_Logic_Vector(bmgwide+2 downto 1);

	begin
	if reset = '1' then
	    bms_q(j)(bmgwide downto 1) <= ms_zero_init;
		survff_q(j) <= '0';
	elsif Rising_edge(clk) then

		-- the trellis_hypo connections needs re-design now with multicode
		bmetrictop(bm_size downto 1) := bmtop_mux_q(j)(bm_size downto 1);				-- +M
	    bmetricbot(bm_size downto 1) := bmbot_mux_q(j)(bm_size downto 1);				-- -M
		bmetrictop(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		bmetricbot(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		smetrictop(bmgwide-1 downto 1) := bms_q(2*j-1)(bmgwide-1 downto 1);
		smetricbot(bmgwide-1 downto 1) := bms_q(2*j  )(bmgwide-1 downto 1);
		smetrictop(bmgwide) := bms_q(2*j-1)(bmgwide) and not overhold_and_q(3);
		smetricbot(bmgwide) := bms_q(2*j  )(bmgwide) and not overhold_and_q(3);
	
		add_metric_top(bmgwide downto 1) :=
							 unsigned(bmetrictop(bmgwide downto 1)) +
							 unsigned(smetrictop(bmgwide downto 1));
		add_metric_bot(bmgwide downto 1) := 
							 unsigned(bmetricbot(bmgwide downto 1)) +
							 unsigned(smetricbot(bmgwide downto 1));

		if ncodes = 1 then
			cmp(bmgwide+2 downto 1) := ('0' & '0' & bms_q(2*j-1)) +
			                           ('1' & '1' & not(bms_q(2*j))) +
			                           ((bmgwide+2 downto bm_size+2 => bmdiff_mux_q(j)(bm_size+1)) & bmdiff_mux_q(j));

			choosetop := cmp(bmgwide+2);
		else
			if unsigned(add_metric_top(bmgwide downto 1)) >
			   unsigned(add_metric_bot(bmgwide downto 1)) then
				choosetop := '0';
			else
				choosetop := '1';
			end if;  
		end if;

		bmsnode(bmgwide downto 1) :=
					(add_metric_top(bmgwide downto 1) and (bmgwide downto 1 => not choosetop)) or
					(add_metric_bot(bmgwide downto 1) and (bmgwide downto 1 => choosetop));

		if val_sink_del(3)='1' then --or clear_bmp(j)='1' then
			survff_q(j) <= choosetop;
		    bms_q(j)(bmgwide downto 1) <= bmsnode(bmgwide downto 1);
		end if;
	end if;
	end process clocking;
end generate ifg5_cnt;


--  I need a condition here for j> than 1 or 0 depending on continuous or not
ifg5 : if j>j_init and j<=halfstates generate


	clocking : Process (clk, reset)

		variable bmsnode : Std_Logic_Vector(bmgwide downto 1);
		variable add_metric_top, add_metric_bot : Std_Logic_Vector(bmgwide downto 1);
		variable choosetop : Std_Logic;
		variable bmetrictop, bmetricbot, smetrictop, smetricbot : Std_Logic_Vector(bmgwide downto 1);
		variable cmp : Std_Logic_Vector(bmgwide+2 downto 1);

	begin
	if reset = '1' then
		 bms_q(j)(bmgwide downto 1) <= (others => '0');
		 survff_q(j) <= '0';
	elsif Rising_edge(clk) then

		-- the trellis_hypo connections needs re-design now with multicode
		bmetrictop(bm_size downto 1) := bmtop_mux_q(j)(bm_size downto 1);				-- +M
	  bmetricbot(bm_size downto 1) := bmbot_mux_q(j)(bm_size downto 1);				-- -M
		bmetrictop(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		bmetricbot(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		smetrictop(bmgwide-1 downto 1) := bms_q(2*j-1)(bmgwide-1 downto 1);
		smetricbot(bmgwide-1 downto 1) := bms_q(2*j  )(bmgwide-1 downto 1);
		smetrictop(bmgwide) := bms_q(2*j-1)(bmgwide) and not overhold_and_q(3);
		smetricbot(bmgwide) := bms_q(2*j  )(bmgwide) and not overhold_and_q(3);
	
		add_metric_top(bmgwide downto 1) :=
							 unsigned(bmetrictop(bmgwide downto 1)) +
							 unsigned(smetrictop(bmgwide downto 1));
		add_metric_bot(bmgwide downto 1) := 
							 unsigned(bmetricbot(bmgwide downto 1)) +
							 unsigned(smetricbot(bmgwide downto 1));
		if ncodes = 1 then
			cmp(bmgwide+2 downto 1) := ('0' & '0' & bms_q(2*j-1)) +
			                           ('1' & '1' & not(bms_q(2*j))) +
			                           ((bmgwide+2 downto bm_size+2 => bmdiff_mux_q(j)(bm_size+1)) & bmdiff_mux_q(j));

			choosetop := cmp(bmgwide+2);
		else
			if unsigned(add_metric_top(bmgwide downto 1)) >
			   unsigned(add_metric_bot(bmgwide downto 1)) then
				choosetop := '0';
			else
				choosetop := '1';
			end if;
		end if;

		bmsnode(bmgwide downto 1) :=
					(add_metric_top(bmgwide downto 1) and (bmgwide downto 1 => not choosetop)) or
					(add_metric_bot(bmgwide downto 1) and (bmgwide downto 1 => choosetop));

		if val_sink_del(3)='1' or clear_bmp(j)='1' then
			survff_q(j) <= choosetop;
			if clear_bmp(j)='0' then
				bms_q(j)(bmgwide downto 1) <= bmsnode(bmgwide downto 1);
			else
				bms_q(j)(bmgwide downto 1) <= bmsnode_ini(j)(bmgwide downto 1);
			end if;
		end if;
	end if;
	end process clocking;
end generate ifg5;


ifg6 : if (j>halfstates) generate

      
	clocking : Process (clk, reset)

		variable bmsnode : Std_Logic_Vector(bmgwide downto 1);
		variable add_metric_top, add_metric_bot : Std_Logic_Vector(bmgwide downto 1);
		variable choosetop : Std_Logic;
		variable bmetrictop, bmetricbot, smetrictop, smetricbot : Std_Logic_Vector(bmgwide downto 1);
		variable cmp : Std_Logic_Vector(bmgwide+2 downto 1);

	begin
	if reset = '1' then
		bms_q(j)(bmgwide downto 1) <= (others => '0');
		survff_q(j) <= '0';
	elsif Rising_edge(clk) then

		bmetrictop(bm_size downto 1) := bmtop_mux_q(j)(bm_size downto 1);
	  bmetricbot(bm_size downto 1) := bmbot_mux_q(j)(bm_size downto 1);
		bmetrictop(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		bmetricbot(bmgwide downto bm_size+1) := (others => '0');  -- zero expansion
		smetrictop(bmgwide-1 downto 1) := bms_q(2*(j-halfstates)-1)(bmgwide-1 downto 1);
		smetricbot(bmgwide-1 downto 1) := bms_q(2*(j-halfstates)  )(bmgwide-1 downto 1);
		smetrictop(bmgwide) := bms_q(2*(j-halfstates)-1)(bmgwide) and not overhold_and_q(3);
		smetricbot(bmgwide) := bms_q(2*(j-halfstates)  )(bmgwide) and not overhold_and_q(3);

		add_metric_top(bmgwide downto 1) :=
							 unsigned(bmetrictop(bmgwide downto 1)) +
							 unsigned(smetrictop(bmgwide downto 1));
		add_metric_bot(bmgwide downto 1) := 
							 unsigned(bmetricbot(bmgwide downto 1)) +
							 unsigned(smetricbot(bmgwide downto 1));

		if ncodes = 1 then 
			cmp(bmgwide+2 downto 1) := ('0' & '0' & bms_q(2*(j-halfstates)-1)) +
			                           ('1' & '1' & not(bms_q(2*(j-halfstates)  ))) +
			                           ((bmgwide+2 downto bm_size+2 => bmdiff_mux_q(j)(bm_size+1)) & bmdiff_mux_q(j));

			choosetop := cmp(bmgwide+2);
		else
			if unsigned(add_metric_top(bmgwide downto 1)) >
			   unsigned(add_metric_bot(bmgwide downto 1)) then
				choosetop := '0';
			else
				choosetop := '1';
			end if;
		end if;

		bmsnode(bmgwide downto 1) :=
					(add_metric_top(bmgwide downto 1) and (bmgwide downto 1 => not choosetop)) or
					(add_metric_bot(bmgwide downto 1) and (bmgwide downto 1 => choosetop));

		if val_sink_del(3)='1' or clear_bmp(j)='1' then
			survff_q(j) <= choosetop;
			if clear_bmp(j)='0' then
				bms_q(j)(bmgwide downto 1) <= bmsnode(bmgwide downto 1);
			else
				bms_q(j)(bmgwide downto 1) <= (others => '0');
			end if;
		end if;
	end if;
	end process clocking;
end generate ifg6;


end generate gbna;


-- if any metric > 1000000 (for soft decisions), normalize all
-- can pipeline this calculation - always split into 2 calculations
over_and(1) <= bms_q(1)(bmgwide);
goa: FOR k IN 2 TO halfstates generate
	over_and(k) <= over_and(k-1) and bms_q(k)(bmgwide);
end generate goa;
over_and(halfstates+1) <= bms_q(halfstates+1)(bmgwide);
gob: FOR k IN (halfstates+2) TO maxstates generate
	over_and(k) <= over_and(k-1) and bms_q(k)(bmgwide);
end generate gob;

process(clk, reset)

begin

if reset='1' then
	overhold_and_q <= (others => '0');
	normalizations_int <= (others => '0');
elsif rising_edge(clk) then
	if val_sink_del(3)='1' then
		overhold_and_q(1) <= over_and(halfstates) and not overhold_and_q(3);
		overhold_and_q(2) <= over_and(maxstates) and not overhold_and_q(3);
	end if;
	if val_sink_del(3)='1' then
		overhold_and_q(3) <= (overhold_and_q(1) and overhold_and_q(2)) and not overhold_and_q(3);
  end if;
	if val_sink_del(3)='1' then
		if eop_sink_del(3)='1' then
			normalizations_int <= (others => '0');
		elsif overhold_and_q(3)='1' then
			normalizations_int <= unsigned(normalizations_int) + natural(1);
		end if;
	end if;
end if;
end process;


ifg_bsf: if best_state_finder="used" generate --or best_state_finder="TRUE" generate

	fg7c : for k in 1 to maxstates generate
	
		bmgsel_q(0)(K)(bmgwide downto 1) <= bms_q(K)(bmgwide downto 1);
		bmgaddsel_q(0)(K)(L_max-1 downto 1) <= --natural_2_m(arg => K-1, size => L_max-1);
		    CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => K-1, SIZE => L_max-1), SIZE => L_max-1);
	
	end generate fg7c;	
	
	
	fg2d: for I in 0 to L_max-2 generate  -- I =0 is the previous generate
		fg3d: for J in 1 to tree_arch(I+1) generate
			comp3: process(bmgsel_q) 
			begin
				if unsigned(bmgsel_q(I)(2*J-1)(bmgwide downto 1)) > unsigned(bmgsel_q(I)(2*J)(bmgwide downto 1)) then
					sel_tree(I)(J) <= '0';
				else
					sel_tree(I)(J) <= '1';
				end if;
			end process comp3;
			bmgsel_d(I+1)(J)(bmgwide downto 1) <= 
			(bmgsel_q(I)(2*J-1)(bmgwide downto 1) and (bmgwide downto 1 => not sel_tree(I)(J))) or
			(bmgsel_q(I)(2*J  )(bmgwide downto 1) and (bmgwide downto 1 => sel_tree(I)(J)));
			bmgaddsel_d(I+1)(J)(I+1) <= sel_tree(I)(J);
	
			ifg33: if I > 0 generate
				bmgaddsel_d(I+1)(J)(I downto 1) <= 
				(bmgaddsel_q(I)(2*J-1)(I downto 1) and (I downto 1 => not sel_tree(I)(J))) or
				(bmgaddsel_q(I)(2*J  )(I downto 1) and (I downto 1 => sel_tree(I)(J)));
			end generate ifg33;
	
			bmgsel : Process(clk, reset)
			begin
			if reset='1' then
				bmgsel_q(I+1)(J)(bmgwide downto 1) <= (others => '0');
				bmgaddsel_q(I+1)(J)(I+1 downto 1) <= (others => '0');
			elsif Rising_edge(clk) then
				if val_sink_del(4+I)='1' then
					bmgsel_q(I+1)(J)(bmgwide downto 1) <= bmgsel_d(I+1)(J)(bmgwide downto 1);
					bmgaddsel_q(I+1)(J)(I+1 downto 1) <= bmgaddsel_d(I+1)(J)(I+1 downto 1);
				end if;
			end if;
			end process bmgsel;          
	
		end generate fg3d;
	end generate fg2d;
	
	bestmet <= bmgsel_q(L_max-1)(1)(bmgwide downto 1);
	bestadd <= bmgaddsel_q(L_max-1)(1)(L_max-1 downto 1);

end generate ifg_bsf;

ifg_no_bsf: if best_state_finder="unused" generate --or best_state_finder="FALSE" generate
  bestmet <= (others => '0');
	bestadd <= (others => '0');
end generate ifg_no_bsf;


normalizations <= normalizations_int;
survive <= survff_q;

end rtl;
