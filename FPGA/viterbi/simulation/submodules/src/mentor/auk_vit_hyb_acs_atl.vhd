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


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.vi_interface.all;
use work.vi_functions.all;
library altera_mf;
use altera_mf.altera_mf_components.all;


Entity auk_vit_hyb_acs_atl is
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
end entity auk_vit_hyb_acs_atl;	


Architecture rtl of auk_vit_hyb_acs_atl is

--Constant config : STRING := "no_max_info";

Constant metwide : NATURAL := softbits+LOG2_ceil_table(n);
Constant log2_ACS_units : NATURAL := LOG2_ceil_table(ACS_units);
Constant tree_arch : NATURAL_ARRAY(0 to log2_ACS_units) := Build_tree_arch(ACS_units); -- (4, 2, 1)
-- log2_cc stands for log2 of the number of clock cicles required to process
-- a set of incoming symbols. As the number of ACS units increases less clock cicles
-- required. There is a limit log2_cc cannot be lower than 3. The reason is that overlaping
-- of the newly states metrics with the previous. For value 3 the actual number  
-- of clock cycles is 10 , not 8 as the sequencer stops for 2 clock cycles 
--Constant log2_cc : NATURAL := L_max-2-LOG2_ceil_table(ACS_units);
Constant L_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => L, ncodes => ncodes);

Constant two : Std_Logic_Vector(log2_cc downto 1) := --natural_2_m(arg => 2, size => log2_cc);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2, SIZE => log2_cc), SIZE => log2_cc);
Constant thr_write : Std_Logic_Vector(log2_cc+1 downto 1) := --natural_2_m(arg => 2**log2_cc-1, size => log2_cc+1);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**log2_cc-1, SIZE => log2_cc+1), SIZE => log2_cc+1);
Constant best_find : Std_Logic_Vector(log2_cc+1 downto 1) := --natural_2_m(arg => 2**log2_cc  , size => log2_cc+1);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**log2_cc, SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant binary_table : Vector_2D(0 to 2*ACS_units-1) := Build_binary_table(2*ACS_units);
Constant binary_table_ncodes : Vector_2D(0 to ncodes-1) := Build_binary_table(ncodes);
Constant binary_table_acs_even : Vector_2D(0 to ACS_units-1) := Build_binary_table_evenodd(ACS_units, 0);
Constant binary_table_acs_odd  : Vector_2D(0 to ACS_units-1) := Build_binary_table_evenodd(ACS_units, 1);


Constant zero : Std_Logic_Vector(log2_cc+1 downto 1) := --natural_2_m(arg => 0, size => log2_cc+1);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 0, SIZE => log2_cc+1), SIZE => log2_cc+1);
Constant vector_zero : Std_Logic_Vector(bmgwide downto 1) := --natural_2_m(arg => 0, size => bmgwide);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 0, SIZE => bmgwide), SIZE => bmgwide);
Constant one : Std_Logic_Vector(log2_cc+1 downto 1) := --natural_2_m(arg => 1, size => log2_cc+1);
               CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 1, SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant latch_best_point : Std_Logic_Vector(log2_cc+1 downto 1) := 
          -- 0 when ACS=4, 8 when ACS=1, 9 when ACS=2, 1 when ACS=8 and 2 when ACS=16
          --natural_2_m(arg => ((8 + log2_ACS_units) mod 10), size => log2_cc+1);
					CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => ((8 + log2_ACS_units) mod 10), SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant latch_best_point2 : Std_Logic_Vector(log2_cc+1 downto 1) := 
          -- This is for when log2_cc > 3
          --natural_2_m(arg => log2_ACS_units, size => log2_cc+1);
					CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => log2_ACS_units, SIZE => log2_cc+1), SIZE => log2_cc+1);

-- if log2_cc=3 then 10-1 , else 2**log2_cc-1
Constant sequencer_count : Std_Logic_Vector(log2_cc+1 downto 1) :=
					set_sequencer_count(arg => log2_cc);

Constant readadd_reset : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 --natural_2_m(arg => 3, size => log2_cc+1);
				 CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 3, SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant readadd_reset2 : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 unsigned(sequencer_count)-natural(6);

Constant allow_ena_time : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 --natural_2_m(arg => 2**(log2_cc+1)-3, size => log2_cc+1);
				 CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**(log2_cc+1)-3, SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant allow_ena_time2 : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 unsigned(sequencer_count) - natural(2);

Constant chk_dav_sink_time : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 --natural_2_m(arg => 2**(log2_cc+1)-2, size => log2_cc+1);
				 CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**(log2_cc+1)-2, SIZE => log2_cc+1), SIZE => log2_cc+1);

Constant chk_dav_sink_time2 : Std_Logic_Vector(log2_cc+1 downto 1) := 
				 unsigned(sequencer_count) - natural(1);

Constant seq_width : NATURAL := set_sequencer_width(arg => log2_cc);
Constant range_2D: NATURAL := Get_max_of_three(3, 2*ACS_units, 0);

--Subtype Vector_2D_ACS is Vector_2D(2*ACS_units downto 1);
Subtype Vector_2D_ACS is Vector_2D(range_2D downto 1);
Type Vector_3D is ARRAY(NATURAL RANGE<>) of Vector_2D_ACS;

signal bmgsel_d, bmgsel_q, bmgaddsel_d, bmgaddsel_q : Vector_3D(log2_ACS_units+1 downto 0);
signal sel_tree : Vector_2D(log2_ACS_units downto 0);
signal oddmem, evenmem : Vector_2D(ACS_units downto 1);
signal hypo_one_top_q, hypo_one_bot_q : Vector_2D(ACS_units downto 1);
signal hypo_nil_top_q, hypo_nil_bot_q : Vector_2D(ACS_units downto 1);
signal selecttop, selectbot, mux_dh1_h0, mux_dh0_h1 : Std_Logic_Vector(ACS_units downto 1);
signal del_hypo_nil_q, del_hypo_one_q : Vector_2D(ACS_units downto 1);
signal hypo_one_sel_d, hypo_nil_sel_d : Vector_2D(ACS_units downto 1);
signal hypo_one_sel_q, hypo_nil_sel_q : Vector_2D(ACS_units downto 1);
signal hold_data_even, hold_data_odd : Vector_2D(ACS_units downto 1);
signal hold_state_even_q, hold_state_odd_q : Vector_2D(ACS_units downto 1);
signal data_odd_in, data_even_in : Vector_2D(ACS_units downto 1);
signal data_odd_pre_in, data_even_pre_in : Vector_2D(ACS_units downto 1);
signal write_even, write_odd : Std_Logic_Vector(ACS_units downto 1);
Signal init_acs_even, init_acs_odd : Std_Logic_Vector(ACS_units downto 1);
signal start_find_best_odd, start_find_best_even : Std_Logic_Vector(ACS_units downto 1);
signal writeeven_seq, writeodd_seq, readadd_seq : Std_Logic_Vector(log2_cc+1 downto 1);
signal writeadd_odd, writeadd_even : Vector_2D(ACS_units downto 1);
Signal state_number, state_number_mux : Vector_2D(range_2D downto 1); 
signal  normctl_q : Std_Logic;
signal nextnorm_enable : Std_Logic_Vector(ACS_units downto 0);
signal we_high_acs_odd, we_low_acs_even, survlatch_node : Std_Logic;
signal init_high_acs_odd, init_low_acs_even : Std_Logic;
signal match_even, match_odd : Std_Logic_Vector(ACS_units downto 1);
signal latch_best_q, latch_best_in, nofirst, butsecond : Std_Logic;
signal normalizations_int : Std_Logic_Vector(8 downto 1);
signal bm_init_state_mux : Std_Logic_Vector(L_max-1 downto 1);
signal acs_end_round : Std_Logic;
signal allow_best_latch, eop_sink_q, we_ram : Std_Logic; -- moved from BMP
signal we_ram_acs, we_ram_sur, acs_init : Std_Logic;
signal we_ram_acs_first, init_acs_first : Std_Logic;
Signal end_wr_round, pre_start_odd, allow_ena_assert, init_acs : Std_Logic;
Signal eop_early_round_ind, eop_later_round_ind : std_logic;
Signal sel_code_early, sel_code_later : Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
Signal allow_hold_best_even, allow_hold_best_odd : std_logic_vector(ACS_units downto 1);
Signal allow_hold_best_odd_premux, allow_hold_best_even_premux : Vector_2D(ncodes downto 1);


Type norm_machine_states is (idle, scan, book_norm, norm);

signal norm_state, next_norm_state : norm_machine_states;
--signal out_fsm_norm : Std_Logic;

Type machine_states is (S0, monitor, used_round, init_round, lost_round, error_op);

signal state, next_state : machine_States;
signal out_fsm : Std_Logic_Vector(2 downto 1);


begin



--- Moved from BMP

allow_best_latch_proc: process(clk, reset)
begin
	if reset='1' then
    ena_master_sink	 <= '0'; 
    allow_best_latch <= '0'; --'1';
		block_delimeters <= '0'; 
		eop_sink_q <= '0';
		eop_early_round_ind <= '0';
		eop_later_round_ind  <= '0';
		sel_code_early <= (others => '0');
		sel_code_later  <= (others => '0');
  elsif Rising_edge(clk) then
	  -- watch out with latch_best, it is an output here
		if latch_best_q='1' then
     	allow_best_latch <= we_ram_sur;
		elsif survlatch_node='1' and we_ram_sur='1' then
			allow_best_latch <= '1';
		end if;
    if latch_best_q='1' then
			block_delimeters <= eop_sink_q;
    end if;
		if end_wr_round='1' then
			eop_early_round_ind <= eop_sink_q;
			sel_code_early <= sel_code;
		end if;
		if pre_start_odd='1' then  -- this is shifted  (for odd in acs=1 , but for even and odd {acs/2 + 1 to acs} for acs>1) 
			eop_later_round_ind <= eop_sink_q;
			sel_code_later <= sel_code;
		end if;
		if val_sink='1' then
		  eop_sink_q <= eop_sink;
		end if;
		ena_master_sink	 <= dav_master_sink and allow_ena_assert;
  end if;
end process allow_best_latch_proc;


----------------------------------------
-- FSM to control/monitor atlantic interface
-- and the bmp process Unit

FSM: process(state, val_sink, eop_sink, acs_end_round, end_wr_round,
             allow_ena_assert)

	begin
		case state is
		
		when S0 => 
			if allow_ena_assert='1' then
				next_state <= monitor;
			elsif acs_end_round='1' then
				next_state <= lost_round;
			elsif val_sink='1' then
				next_state <= error_op;
			else
				next_state <= S0;
			end if;
		when monitor => 
			if val_sink='1' and eop_sink='1' then
				next_state <= init_round;
			elsif val_sink='1' and eop_sink='0' then
				next_state <= used_round;
			elsif val_sink='0' and  acs_end_round='1' then
				next_state <= lost_round;
			else
				next_state <= monitor;
			end if;
		when init_round => 
			if end_wr_round='1' then
				next_state <= S0;
			else
				next_state <= init_round;
			end if;
		when used_round => 
			if end_wr_round='1' then
				next_state <= S0;
			else
				next_state <= used_round;
			end if;
		when lost_round => 
			if allow_ena_assert='1' then
				next_state <= monitor;
			else
				next_state <= lost_round;
			end if;
			
		when others => next_state <= S0;
		end case;
		
	end process FSM;

clk_FSM: Process (clk, reset)
	begin
		if reset='1' then
			state <= S0;
		elsif Rising_edge(clk) then
			state <= next_state;
		end if;
		
end process clk_FSM;


acs_init    <= out_fsm(1);
we_ram      <= out_fsm(2);


outputs_FSM: process(state)

	begin
		case state is

		when S0 =>
			out_fsm <= "00";
		when monitor =>
			out_fsm <= "00";
    when used_round =>
			out_fsm <= "10";
		when init_round =>
			out_fsm <= "11";
		when lost_round =>
			out_fsm <= "00";
		when error_op =>
			out_fsm <= "00";
		when others => 
			out_fsm <= "00";
		end case;
		
end process outputs_FSM;

survlatch_reg : Process (clk, reset)
begin
if reset = '1' then
	survready_q <= '0';
elsif Rising_edge(clk) then
	survready_q <= survlatch_node  and we_ram_sur;
end if;
end process survlatch_reg;


-- I am afraid I will need different "kick-start" mechanism
-- depending on log2_cc being =3 or >3


--- end of logic moved from BMP



FSM_norm: process(norm_state, nextnorm_enable(ACS_units), acs_end_round, we_ram_acs)

	begin
		case norm_state is
		when idle =>
			if nextnorm_enable(ACS_units)='1' and acs_end_round='1' and we_ram_acs='1' then
				next_norm_state <= scan;
			else
				next_norm_state <= idle;
			end if;
		when scan =>
			if nextnorm_enable(ACS_units)='1' and acs_end_round='1' and we_ram_acs='1' then
				next_norm_state <= norm;
			elsif nextnorm_enable(ACS_units)='1' and acs_end_round='1' and we_ram_acs='0' then
				next_norm_state <= book_norm;
			elsif nextnorm_enable(ACS_units)='1' and acs_end_round='0' then
				next_norm_state <= scan;
			else 
				next_norm_state <= idle;
			end if;
		when book_norm =>
			if acs_end_round='1' and we_ram_acs='1' then
				next_norm_state <= norm;
			else
				next_norm_state <= book_norm;
			end if;
		when norm =>
			if acs_end_round='1' and we_ram_acs='1' then
				next_norm_state <= idle;
			else
				next_norm_state <= norm;
			end if;
		when others => next_norm_state <= idle;
		end case;
		
	end process FSM_norm;

clk_FSM_norm: Process (clk, reset)
	begin
		if reset='1' then
			norm_state <= idle;
		elsif Rising_edge(clk) then
			norm_state <= next_norm_state;
		end if;
		
end process clk_FSM_norm;


outputs_FSM_norm: process(norm_state)

	begin
		case norm_state is
		when idle =>
			normctl_q <= '0';
		when scan =>
			normctl_q <= '0';
		when book_norm =>
			normctl_q <= '0';
		when norm =>
			normctl_q <= '1';
		when others => 
			normctl_q <= '0';
		end case;
		
end process outputs_FSM_norm;


-- VCC_signal <= '1';


pre_state_vitenca <= unsigned(readadd_seq)-unsigned(one);

-- moved to metric unit
--ifg32: if ACS_units=1 generate
--
--	state_vitenca(1)(1) <= '0';
--	state_vitenca(1)(L_max-1 downto log2_ACS_units+2) <= pre_state_vitenca(log2_cc downto 1);
--	state_vitenca(1)(L_max) <= '0';
--
--end generate ifg32;
--
--ifg33: if ACS_units>1 generate
--	fg33 :  For K in 1 to ACS_units generate
--
--		state_vitenca(K)(1) <= '0';
--		state_vitenca(K)(log2_ACS_units+1 downto 2) <= natural_2_m(arg => K-1, size => log2_ACS_units);
--		state_vitenca(K)(L_max-1 downto log2_ACS_units+2) <= pre_state_vitenca(log2_cc downto 1);
--		state_vitenca(K)(L_max) <= '0';
--
--	end generate fg33;
--end generate ifg33;


--***************
--*** Control ***
--***************

ifg9: if log2_cc>3 generate

	sequencing: process(readadd_seq, allow_ena_assert_from_trb)

	begin
		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(two) then
			survlatch_node <= '1';
		else
			survlatch_node <= '0';
		end if;
--		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(sequencer_count) then
  -- Watch out for effect on FSM at BMP for DAV toggling I have to 
	-- change to value 1 because of normalization mis-aligned is degradding metrics later on
		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(one) then
			acs_end_round <= '1';
		else
			acs_end_round <= '0';
		end if;	
--		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(allow_ena_time) then
		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(allow_ena_time(log2_cc downto 1)) then
			allow_ena_assert <= allow_ena_assert_from_trb;
		else
			allow_ena_assert <= '0';
		end if;
		-- I will have to generate a sequencer the carrousel type instead of so many comparators.	
		if unsigned(readadd_seq(log2_cc downto 1))=unsigned(chk_dav_sink_time(log2_cc downto 1)) then
			check_dav_sink <= '1';
		else
			check_dav_sink <= '0';
		end if;	
		
	end process sequencing;

end generate ifg9;

ifg10: if log2_cc=3 generate

	sequencing: process(readadd_seq, allow_ena_assert_from_trb)

	begin
		if unsigned(readadd_seq)=unsigned(zero) then
			survlatch_node <= '1';
		else
			survlatch_node <= '0';
		end if;
		if unsigned(readadd_seq)=unsigned(sequencer_count) then
			acs_end_round <= '1';
		else
			acs_end_round <= '0';
		end if;
		if unsigned(readadd_seq)=unsigned(allow_ena_time2) then
			allow_ena_assert <= allow_ena_assert_from_trb;
		else
			allow_ena_assert <= '0';
		end if;	
		if unsigned(readadd_seq)=unsigned(chk_dav_sink_time2) then
			check_dav_sink <= '1';
		else
			check_dav_sink <= '0';
		end if;		
	end process sequencing;

end generate ifg10;


-- As now control is executed from the FSM,
-- the FSM will control the writting on the memories
-- there is a 1 clock cycle delay between _odd and _even
no_first_2: process(clk, reset)

begin
	if reset='1' then
		we_high_acs_odd <='0';
		--we_low_acs_even <='0';
		init_high_acs_odd <='0';
		--init_low_acs_even <='0';
	elsif Rising_edge(clk) then

			--we_low_acs_even <= we_ram_acs;
			we_high_acs_odd <= we_low_acs_even;
			--init_low_acs_even <= init_acs;
			init_high_acs_odd <=init_low_acs_even;

	end if;
end process no_first_2;

we_low_acs_even <= we_ram_acs;
init_low_acs_even <= init_acs;

if_acs1: if ACS_units=1 generate

ifg11: if log2_cc=3 generate
	write_proc: process(writeodd_seq, writeeven_seq,  
	                    we_high_acs_odd, init_high_acs_odd, we_low_acs_even, init_low_acs_even)

	begin
		-- zero should be replaced by latch_best_point 
		-- this var ought to be zero when ACS=4 and be (x - 2) mod 10 = 8 when ACS=1 
		--if unsigned(writeeven_seq) = unsigned(zero) then
		if unsigned(writeeven_seq) = unsigned(latch_best_point) then
			latch_best_in <= '1';
--			latch_best_mark <= '1';
		else
			latch_best_in <= '0';
--			latch_best_mark <= '0';
		end if; 
		if unsigned(writeodd_seq) > unsigned(thr_write) then
			write_odd(1) <= '0';
			init_acs_odd(1) <= '0';
		else
			write_odd(1) <= we_high_acs_odd;
			init_acs_odd(1) <= init_high_acs_odd;
		end if;
		if unsigned(writeeven_seq) > unsigned(thr_write) then
			write_even(1) <= '0';
			init_acs_even(1) <= '0';
		else
			write_even(1) <= we_low_acs_even;
			init_acs_even(1) <= init_low_acs_even;
		end if; 
	end process write_proc;

end generate ifg11;

ifg12: if log2_cc>3 generate
	sec_proc: process(writeeven_seq)
	begin
		if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(latch_best_point2(log2_cc downto 1)) then
			latch_best_in <= '1';
		else
			latch_best_in <= '0';
		end if; 
	end process sec_proc;
	
	write_odd(1) <= we_high_acs_odd;
	write_even(1) <= we_low_acs_even;
	init_acs_odd(1) <= init_high_acs_odd;
	--init_acs_even(1) <= init_low_acs_even;
	-- there is 1 clock cicle overlap
	init_acs_even(1) <= init_acs;
		

	writeadd_even(1)(log2_cc+1) <= writeeven_seq(log2_cc+1);
	writeadd_odd(1)(log2_cc+1) <= writeodd_seq(log2_cc+1);
end generate ifg12;

	writeadd_even(1)(log2_cc) <= writeeven_seq(1);
	writeadd_even(1)(log2_cc-1 downto 1) <= writeeven_seq(log2_cc downto 2);
	writeadd_odd(1)(log2_cc) <= not writeodd_seq(1);
	writeadd_odd(1)(log2_cc-1 downto 1) <= writeodd_seq(log2_cc downto 2);


some_logic: process(writeodd_seq, writeeven_seq)

	begin
		if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(thr_write(log2_cc downto 1)) then
			end_wr_round <= '1';
		else
			end_wr_round <= '0';
		end if; 
		if unsigned(writeodd_seq(log2_cc downto 1)) = unsigned(thr_write(log2_cc downto 1)) then
			pre_start_odd <= '1';
		else
			pre_start_odd <= '0';
		end if; 
		if unsigned(writeodd_seq(log2_cc downto 1)) = unsigned(best_find(log2_cc downto 1)) then
			start_find_best_odd(1) <= '1';
		else
			start_find_best_odd(1) <= '0';
		end if;
		--if unsigned(writeeven_seq) = unsigned(thr_write) then
		if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(best_find(log2_cc downto 1)) then
			start_find_best_even(1) <= '1';
		else
			start_find_best_even(1) <= '0';
		end if; 
	end process some_logic;

end generate if_acs1;


if_acs2: if ACS_units>=2 generate

ifg81: if log2_cc=3 generate
	write_proc: process(writeodd_seq, writeeven_seq,  
	                    we_low_acs_even, init_low_acs_even, we_high_acs_odd, init_high_acs_odd)

	begin
		-- zero should be replaced by latch_best_point 
		-- this var ought to be zero when ACS=4 and be (x - 2) mod 10 = 8 when ACS=1 
		--if unsigned(writeeven_seq) = unsigned(zero) then
		if unsigned(writeeven_seq) = unsigned(latch_best_point) then
			latch_best_in <= '1';
--			latch_best_mark <= '1';
		else
			latch_best_in <= '0';
--			latch_best_mark <= '0';
		end if; 
		if unsigned(writeeven_seq) > unsigned(thr_write) then
			for I in 1 to ACS_units/2 loop
				write_even(I) <= '0';
				write_odd(I) <= '0';
				init_acs_odd(I) <= '0';
				init_acs_even(I) <= '0';
			end loop;
		else
			for I in 1 to ACS_units/2 loop
				write_even(I) <= we_low_acs_even;
				write_odd(I) <= we_low_acs_even;
				init_acs_odd(I) <= init_low_acs_even;
				init_acs_even(I) <= init_low_acs_even;
			end loop;
		end if;
		if unsigned(writeodd_seq) > unsigned(thr_write) then
			for I in ACS_units/2+1 to ACS_units loop
				write_even(I) <= '0';
				write_odd(I) <= '0';
				init_acs_odd(I) <= '0';
				init_acs_even(I) <= '0';
			end loop;
		else
			for I in ACS_units/2+1 to ACS_units loop
				write_even(I) <= we_high_acs_odd;
				write_odd(I) <= we_high_acs_odd;
				init_acs_odd(I) <= init_high_acs_odd;
				init_acs_even(I) <= init_high_acs_odd;
			end loop;
		end if; 
	end process write_proc;
end generate ifg81;

ifg82: if log2_cc>3 generate
	sec_proc: process(writeeven_seq)
	begin
		if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(latch_best_point2(log2_cc downto 1)) then
			latch_best_in <= '1';
		else
			latch_best_in <= '0';
		end if; 
	end process sec_proc;

	fg882: for I in 1 to ACS_units/2 generate
		write_odd(I) <= we_low_acs_even;
		write_even(I) <= we_low_acs_even;
		-- Probably 1 clock overlap here as well
		init_acs_odd(I) <= init_low_acs_even;
		init_acs_even(I) <= init_low_acs_even;
		writeadd_even(I)(log2_cc+1) <= writeeven_seq(log2_cc+1);
		writeadd_odd(I)(log2_cc+1) <= writeeven_seq(log2_cc+1);
	end generate fg882;
	fg883: for I in ACS_units/2+1 to ACS_units generate
		write_odd(I) <= we_high_acs_odd;
		write_even(I) <= we_high_acs_odd;
		-- Probably 1 clock overlap here as well
		init_acs_odd(I) <= init_high_acs_odd;
		init_acs_even(I) <= init_high_acs_odd;
		writeadd_even(I)(log2_cc+1) <= writeodd_seq(log2_cc+1);
		writeadd_odd(I)(log2_cc+1) <= writeodd_seq(log2_cc+1);
	end generate fg883;
end generate ifg82;

fg884: for I in 1 to ACS_units/2 generate
	writeadd_even(I)(log2_cc) <= writeeven_seq(1);
	writeadd_even(I)(log2_cc-1 downto 1) <= writeeven_seq(log2_cc downto 2);
	writeadd_odd(I)(log2_cc) <= writeeven_seq(1);
	writeadd_odd(I)(log2_cc-1 downto 1) <= writeeven_seq(log2_cc downto 2);
end generate fg884;

fg885: for I in ACS_units/2+1 to ACS_units generate
	writeadd_even(I)(log2_cc) <= not writeodd_seq(1);
	writeadd_even(I)(log2_cc-1 downto 1) <= writeodd_seq(log2_cc downto 2);
	writeadd_odd(I)(log2_cc) <= not writeodd_seq(1);
	writeadd_odd(I)(log2_cc-1 downto 1) <= writeodd_seq(log2_cc downto 2);
end generate fg885;

some_logic: process(writeodd_seq, writeeven_seq)

	begin
		if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(thr_write(log2_cc downto 1)) then
			end_wr_round <= '1';
		else
			end_wr_round <= '0';
		end if;
		if unsigned(writeodd_seq(log2_cc downto 1)) = unsigned(thr_write(log2_cc downto 1)) then
			pre_start_odd <= '1';
		else
			pre_start_odd <= '0';
		end if; 
		for I in 1 to ACS_units/2 loop
			if unsigned(writeeven_seq(log2_cc downto 1)) = unsigned(best_find(log2_cc downto 1)) then
				start_find_best_odd(I) <= '1';
				start_find_best_even(I) <= '1';
			else
				start_find_best_odd(I) <= '0';
				start_find_best_even(I) <= '0';
			end if;
		end loop;
		for I in ACS_units/2+1 to ACS_units loop
			if unsigned(writeodd_seq(log2_cc downto 1)) = unsigned(best_find(log2_cc downto 1)) then
				start_find_best_odd(I) <= '1';
				start_find_best_even(I) <= '1';
			else
				start_find_best_odd(I) <= '0';
				start_find_best_even(I) <= '0';
			end if; 
		end loop;
	end process some_logic;

end generate if_acs2;


--****************
--*** Counters ***
--****************

ifg3: if log2_cc>3 generate

	readadd_atl : Process (clk, reset)

		variable readadd_seq_var : Std_Logic_Vector(log2_cc+1 downto 1);

	begin
	if reset = '1' then
		readadd_seq <= readadd_reset;
	elsif Rising_edge(clk) then
		readadd_seq_var := readadd_seq;
		readadd_seq_var := readadd_seq_var + natural(1);
		readadd_seq(log2_cc downto 1) <= readadd_seq_var(log2_cc downto 1);
		if state /= lost_round and we_high_acs_odd='1' then
			readadd_seq(log2_cc+1) <= readadd_seq_var(log2_cc+1);
		end if;
	end if;
	end process readadd_atl;


	writeeven_atl : Process (clk, reset)

    variable writeeven_seq_var : Std_Logic_Vector(log2_cc+1 downto 1);

	begin
	if reset = '1' then
		-- I need to change this starting value depending on L , ACS combination!!
		writeeven_seq <= --natural_2_m(arg => 2**log2_cc - 1, size => log2_cc+1);
		     CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**log2_cc - 1, SIZE => log2_cc+1), SIZE => log2_cc+1);
	elsif Rising_edge(clk) then
	  writeeven_seq_var := writeeven_seq;
		writeeven_seq_var := writeeven_seq_var + natural(1);
		writeeven_seq(log2_cc downto 1) <= writeeven_seq_var(log2_cc downto 1);
		-- this is no good creates more errors than it solves!!
		if we_low_acs_even='1' then
			writeeven_seq(log2_cc+1) <= writeeven_seq_var(log2_cc+1);
		end if;
	end if;
	end process writeeven_atl;


	writeodd_atl : Process (clk, reset)

		variable writeodd_seq_var : Std_Logic_Vector(log2_cc+1 downto 1);

	begin
	if reset = '1' then
		writeodd_seq <= --natural_2_m(arg => 2**log2_cc - 2, size => log2_cc+1);
		       CONV_STD_LOGIC_VECTOR(ARG => CONV_UNSIGNED(ARG => 2**log2_cc - 2, SIZE => log2_cc+1), SIZE => log2_cc+1);
		-- need to change due to L, ACS combination
	elsif Rising_edge(clk) then
		writeodd_seq_var := writeodd_seq;
		writeodd_seq_var := writeodd_seq_var + natural(1);
		writeodd_seq(log2_cc downto 1) <= writeodd_seq_var(log2_cc downto 1);
		if we_high_acs_odd='1' then
			writeodd_seq(log2_cc+1) <= writeodd_seq_var(log2_cc+1);
		end if;
	end if;
	end process writeodd_atl;

end generate ifg3;


ifg4: if log2_cc=3 generate

	readadd_atl : Process (clk, reset)
	begin
	if reset = '1' then
		readadd_seq <= readadd_reset2;
	elsif Rising_edge(clk) then
		if unsigned(sequencer_count)=unsigned(readadd_seq) then 
			readadd_seq <= (others => '0');
		else
			readadd_seq <= readadd_seq + natural(1);
		end if;
	end if;
	end process readadd_atl;
						

	writeeven_atl : Process (clk, reset)
	begin
	if reset = '1' then
		writeeven_seq <= sequencer_count;
	elsif Rising_edge(clk) then
		if unsigned(sequencer_count)=unsigned(writeeven_seq) then 
			writeeven_seq <= (others => '0');
		else
			writeeven_seq <= writeeven_seq + natural(1);
		end if;
	end if;
	end process writeeven_atl;

	writeodd_atl : Process (clk, reset)
	begin
	if reset = '1' then
		writeodd_seq <= unsigned(sequencer_count)-natural(1);
	elsif Rising_edge(clk) then
		if unsigned(sequencer_count)=unsigned(writeodd_seq) then 
			writeodd_seq <= (others => '0');
		else
			writeodd_seq <= writeodd_seq + natural(1);
		end if;
	end if;
	end process writeodd_atl;

end generate ifg4;



--****************
--*** Memories ***
--****************

ifg_acs1: if ACS_units=1 generate

	mux_dh0_h1(1) <= not readadd_seq(1);
	mux_dh1_h0(1) <= readadd_seq(1);
	
	data_even_pre_in(1)(bmgwide downto 1) <=
					(hypo_nil_sel_q(1)(bmgwide downto 1) and (bmgwide downto 1 => mux_dh0_h1(1))) or
					(del_hypo_one_q(1)(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh0_h1(1)));

	data_odd_pre_in(1)(bmgwide downto 1) <=
					(hypo_one_sel_q(1)(bmgwide downto 1) and (bmgwide downto 1 => mux_dh1_h0(1))) or
					(del_hypo_nil_q(1)(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh1_h0(1)));

	ifg9e: if sm_init_logic="unused" generate
		data_even_in(1)(bmgwide downto 1) <= data_even_pre_in(1)(bmgwide downto 1);
		data_odd_in(1)(bmgwide downto 1) <= data_odd_pre_in(1)(bmgwide downto 1);
  end generate ifg9e;

	ifg9f: if sm_init_logic="used" generate

  data_even_in(1)(bmgwide downto 1) <=
					(data_even_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_even(1))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_even(1)) and 
					(bmgwide downto 1 => init_acs_even(1))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_even(1)) and 
					(bmgwide downto 1 => init_acs_even(1)));
	data_odd_in(1)(bmgwide downto 1) <=
					(data_odd_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_odd(1))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_odd(1)) and 
					(bmgwide downto 1 => init_acs_odd(1))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_odd(1)) and 
					(bmgwide downto 1 => init_acs_odd(1)));
	end generate ifg9f;

end generate ifg_acs1;


ifg_acs8: if ACS_units>=2 generate

fg886: for I in 1 to ACS_units/2 generate

	mux_dh0_h1(I) <= not readadd_seq(1);
	mux_dh1_h0(I) <= not readadd_seq(1);

  data_even_pre_in(I)(bmgwide downto 1) <=
					(hypo_nil_sel_q(2*I-1)(bmgwide downto 1) and (bmgwide downto 1 => mux_dh0_h1(I))) or
					(del_hypo_one_q(2*I-1)(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh0_h1(I)));
	data_odd_pre_in(I)(bmgwide downto 1) <=
					(hypo_nil_sel_q(2*I)(bmgwide downto 1) and (bmgwide downto 1 => mux_dh1_h0(I))) or
					(del_hypo_one_q(2*I)(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh1_h0(I)));
	
	ifg9a: if sm_init_logic="unused" generate
		data_even_in(I)(bmgwide downto 1) <= data_even_pre_in(I)(bmgwide downto 1);
		data_odd_in(I)(bmgwide downto 1) <= data_odd_pre_in(I)(bmgwide downto 1);
  end generate ifg9a;

-- this 4 way mux should be sorrounded by generic to select if option of selecting start state is to
-- be used. Save a few LE. Although for SOPC builder it will be "standard" feature

	ifg9b: if sm_init_logic="used" generate

  data_even_in(I)(bmgwide downto 1) <=
					(data_even_pre_in(I)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_even(I))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_even(I)) and 
					(bmgwide downto 1 => init_acs_even(I))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_even(I)) and 
					(bmgwide downto 1 => init_acs_even(I)));
	data_odd_in(I)(bmgwide downto 1) <=
					(data_odd_pre_in(I)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_odd(I))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_odd(I)) and 
					(bmgwide downto 1 => init_acs_odd(I))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_odd(I)) and 
					(bmgwide downto 1 => init_acs_odd(I)));
	end generate ifg9b;

end generate fg886;

fg887: for I in ACS_units/2+1 to ACS_units generate

	mux_dh0_h1(I) <= readadd_seq(1);
	mux_dh1_h0(I) <= readadd_seq(1);

  data_even_pre_in(I)(bmgwide downto 1) <=
					(hypo_one_sel_q(2*(I-ACS_units/2)-1)(bmgwide downto 1) and (bmgwide downto 1 => mux_dh0_h1(I))) or
					(del_hypo_nil_q(2*(I-ACS_units/2)-1)(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh0_h1(I)));
	data_odd_pre_in(I)(bmgwide downto 1) <=
					(hypo_one_sel_q(2*(I-ACS_units/2))(bmgwide downto 1) and (bmgwide downto 1 => mux_dh1_h0(I))) or
					(del_hypo_nil_q(2*(I-ACS_units/2))(bmgwide downto 1) and (bmgwide downto 1 => not mux_dh1_h0(I)));
	
	ifg9c: if sm_init_logic="unused" generate
  	data_even_in(I)(bmgwide downto 1) <= data_even_pre_in(I)(bmgwide downto 1);
		data_odd_in(I)(bmgwide downto 1) <= data_odd_pre_in(I)(bmgwide downto 1);
  end generate ifg9c;

  ifg9d: if sm_init_logic="used" generate

  data_even_in(I)(bmgwide downto 1) <=
					(data_even_pre_in(I)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_even(I))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_even(I)) and 
					(bmgwide downto 1 => init_acs_even(I))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_even(I)) and 
					(bmgwide downto 1 => init_acs_even(I)));
	data_odd_in(I)(bmgwide downto 1) <=
					(data_odd_pre_in(I)(bmgwide downto 1) and (bmgwide downto 1 => not init_acs_odd(I))) or
					(          vector_zero(bmgwide downto 1) and (bmgwide downto 1 => not match_odd(I)) and 
					(bmgwide downto 1 => init_acs_odd(I))) or
					(        bm_init_value(bmgwide downto 1) and (bmgwide downto 1 => match_odd(I)) and 
					(bmgwide downto 1 => init_acs_odd(I)));

  end generate ifg9d;

end generate fg887;

end generate ifg_acs8;


--****************
--*** Memories ***
--****************


fg8: for K in 1 to acs_units generate

	even_mem: altsyncram 
   GENERIC map (
      operation_mode => "DUAL_PORT", width_a => bmgwide, widthad_a => seq_width, numwords_a => 2**seq_width,
      outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
      outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
      wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, address_reg_b => "CLOCK0",
			width_b => bmgwide, widthad_b => seq_width, numwords_b => 2**seq_width,
			rdcontrol_reg_b => "UNUSED",
			outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
			indata_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
			indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
      read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO",
      intended_device_family => dev_family, lpm_hint => "UNUSED")
   PORT map (
      wren_a => write_even(K), data_a => data_even_in(K)(bmgwide downto 1),
      address_a => writeadd_even(K)(seq_width downto 1),
			address_b => readadd_seq(seq_width downto 1), clock0 => clk, 
			--aclr0 => reset, 
      q_b => evenmem(K)(bmgwide downto 1) );

	odd_mem: altsyncram 
   GENERIC map (
      operation_mode => "DUAL_PORT", width_a => bmgwide, widthad_a => seq_width, numwords_a => 2**seq_width,
      outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
      outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
      wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, address_reg_b => "CLOCK0",
			width_b => bmgwide, widthad_b => seq_width, numwords_b => 2**seq_width,
			rdcontrol_reg_b => "UNUSED",
			outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
			indata_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
			indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
      read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO",
      intended_device_family => dev_family, lpm_hint => "UNUSED")
   PORT map (
      wren_a => write_odd(K), data_a => data_odd_in(K)(bmgwide downto 1),
      address_a => writeadd_odd(K)(seq_width downto 1),
			address_b => readadd_seq(seq_width downto 1), clock0 => clk, 
      q_b => oddmem(K)(bmgwide downto 1) );

end generate fg8;

--***********
--*** ACS ***
--***********



contenders: process(clk, reset)

	variable hypo_one_top_d, hypo_one_bot_d : Vector_2D(ACS_units downto 1);
	variable hypo_nil_top_d, hypo_nil_bot_d : Vector_2D(ACS_units downto 1);

begin
	if reset = '1' then
		For K in 1 to ACS_units loop
			hypo_one_top_q(K)(bmgwide downto 1) <= (others => '0');
			hypo_one_bot_q(K)(bmgwide downto 1) <= (others => '0');
			hypo_nil_top_q(K)(bmgwide downto 1) <= (others => '0');
			hypo_nil_bot_q(K)(bmgwide downto 1) <= (others => '0');
		end loop;
	elsif Rising_edge(clk) then

--bmtoptop -> bm_hypo_nil_top, bmtopbot -> bm_hypo_nil_bot,
--bmbottop -> bm_hypo_one_top, bmbotbot -> bm_hypo_one_bot,

		For K in 1 to ACS_units loop
			hypo_nil_top_d(k)(bmgwide downto 1) :=
			     unsigned(evenmem(k)(bmgwide downto 1)) + unsigned(bm_hypo_nil_top(k)(metwide downto 1));
			hypo_nil_bot_d(k)(bmgwide downto 1) := 
					 unsigned(oddmem(k)(bmgwide downto 1)) + unsigned(bm_hypo_nil_bot(k)(metwide downto 1));
			hypo_one_top_d(k)(bmgwide downto 1) := 
					 unsigned(evenmem(k)(bmgwide downto 1)) + unsigned(bm_hypo_one_top(k)(metwide downto 1));
			hypo_one_bot_d(k)(bmgwide downto 1) := 
					 unsigned(oddmem(k)(bmgwide downto 1)) + unsigned(bm_hypo_one_bot(k)(metwide downto 1));

			hypo_one_top_d(k)(bmgwide) := hypo_one_top_d(k)(bmgwide) and not normctl_q;
			hypo_one_bot_d(k)(bmgwide) := hypo_one_bot_d(k)(bmgwide) and not normctl_q;
			hypo_nil_top_d(k)(bmgwide) := hypo_nil_top_d(k)(bmgwide) and not normctl_q;
			hypo_nil_bot_d(k)(bmgwide) := hypo_nil_bot_d(k)(bmgwide) and not normctl_q;
			hypo_one_top_q(K)(bmgwide downto 1) <= hypo_one_top_d(k)(bmgwide downto 1);
			hypo_one_bot_q(K)(bmgwide downto 1) <= hypo_one_bot_d(k)(bmgwide downto 1);
			hypo_nil_top_q(K)(bmgwide downto 1) <= hypo_nil_top_d(k)(bmgwide downto 1);
			hypo_nil_bot_q(K)(bmgwide downto 1) <= hypo_nil_bot_d(k)(bmgwide downto 1);
		end loop;

	end if;

end process contenders;

fg6: For K in 1 to ACS_units generate

-- for soft decisions, a>b (agb)
comparisons: process(hypo_one_top_q, hypo_one_bot_q, hypo_nil_top_q, hypo_nil_bot_q)
begin
	if unsigned(hypo_one_top_q(k)(bmgwide downto 1)) > unsigned(hypo_one_bot_q(k)(bmgwide downto 1)) then
		selectbot(k) <= '1';
	else
		selectbot(k) <= '0';
	end if;
	if unsigned(hypo_nil_top_q(k)(bmgwide downto 1)) > unsigned(hypo_nil_bot_q(k)(bmgwide downto 1)) then
		selecttop(k) <= '1';
	else
		selecttop(k) <= '0';
	end if;	
end process comparisons;

hypo_one_sel_d(k)(bmgwide downto 1) <=
				(hypo_one_top_q(k)(bmgwide downto 1) and (bmgwide downto 1 => selectbot(k))) or
				(hypo_one_bot_q(k)(bmgwide downto 1) and (bmgwide downto 1 => not selectbot(k)));
hypo_nil_sel_d(k)(bmgwide downto 1) <=
				(hypo_nil_top_q(k)(bmgwide downto 1) and (bmgwide downto 1 => selecttop(k))) or
				(hypo_nil_bot_q(k)(bmgwide downto 1) and (bmgwide downto 1 => not selecttop(k)));


end generate fg6;

selected: process(clk, reset)

begin
	if reset = '1' then
		For K in 1 to ACS_units loop
			hypo_one_sel_q(K)(bmgwide downto 1) <= (others => '0');
			hypo_nil_sel_q(K)(bmgwide downto 1) <= (others => '0');
		end loop;
	elsif Rising_edge(clk) then

		For K in 1 to ACS_units loop
			hypo_one_sel_q(K)(bmgwide downto 1) <= hypo_one_sel_d(K)(bmgwide downto 1);
			hypo_nil_sel_q(K)(bmgwide downto 1) <= hypo_nil_sel_d(K)(bmgwide downto 1);
		end loop;

	end if;

end process selected;

del_sel_h_one : Process(clk, reset)
begin
if reset='1' then
	For K in 1 to ACS_units loop
  	del_hypo_one_q(k) <= (others => '0');
	end loop;
elsif Rising_edge(clk) then
  if readadd_seq(1)='0' then
		For K in 1 to ACS_units loop
    	del_hypo_one_q(k) <= hypo_one_sel_q(k);
		end loop;
  end if;
end if;
end process del_sel_h_one;           

del_sel_h_nil : Process(clk, reset)
begin
if reset='1' then
	For K in 1 to ACS_units loop
  	del_hypo_nil_q(k) <= (others => '0');
	end loop;
elsif Rising_edge(clk) then
  if readadd_seq(1)='1' then
		For K in 1 to ACS_units loop
    	del_hypo_nil_q(k) <= hypo_nil_sel_q(k);
		end loop;
  end if;
end if;
end process del_sel_h_nil;


nextnorm_enable(0) <= '1';
fg7 :  For K in 1 to ACS_units generate

	nextnorm_enable(K) <= nextnorm_enable(K-1) and (hypo_one_sel_q(K)(bmgwide) and hypo_nil_sel_q(K)(bmgwide));	

end generate fg7;

--normctl_q <= out_fsm;

----*****************************
----*** Calculate Best Metric ***
----*****************************


-- Modifications to sort out issue SPR 195369
-- Modifying the best state finder to find only the best state out of a subset of states
-- depending on the L(i) vs L_max

ifg_spr1: if ncodes=1 generate

	-- Duong: Modifications to sort out issue SPR 281174
	-- the core implemented with L_max = 5 even though L = 3 or 4
	-- I should seperate two cases, one with L = L_max, L < L_max
	-- other wise, the core will not work with L < L_max in single code set mode
	
	-- This case L = L_max	
	ifg_spr1a: if Convert_str_nat(pol_value => L, base => "DEC")-1 > log2_cc generate
		
		filter_states1a: Process(writeadd_even, writeadd_odd, tr_init_state)
			variable writeadd_even_exp, writeadd_odd_exp : std_logic_vector(L_max-1 downto 1);
		begin
			-- at the moment non-generic code
			For K in 1 to ACS_units loop
				--  I need a constant binary table the size of log2_ACS_units+1 split
				-- in even and odd numbers.
				--  binary_table_acs_odd
				writeadd_even_exp(L_max-1 downto L_max-log2_cc) := writeadd_even(k)(log2_cc downto 1);
				writeadd_even_exp(log2_ACS_units+1 downto 1) := binary_table_acs_even(k-1)(log2_ACS_units+1 downto 1);
				writeadd_odd_exp(L_max-1 downto L_max-log2_cc) := writeadd_odd(k)(log2_cc downto 1);
				writeadd_odd_exp(log2_ACS_units+1 downto 1) := binary_table_acs_odd(k-1)(log2_ACS_units+1 downto 1);
				
				if unsigned(tr_init_state(L_max-1 downto 1)) = unsigned(writeadd_even_exp(L_max-1 downto 1)) then
					allow_hold_best_even(k) <= '1';
				else
					allow_hold_best_even(k) <= '0';
				end if;
				
				if unsigned(tr_init_state(L_max-1 downto 1)) = unsigned(writeadd_odd_exp(L_max-1 downto 1)) then
					allow_hold_best_odd(k) <= '1';
				else
					allow_hold_best_odd(k) <= '0';
				end if;
			end loop;
		end process filter_states1a;

	end generate ifg_spr1a;
	
	
	-- this case when L < L_max
	ifg_spr1b: if Convert_str_nat(pol_value => L, base => "DEC")-1 <= log2_cc generate
		
		filter_states1b: Process(writeadd_even, writeadd_odd, tr_init_state)
			variable writeadd_even_exp, writeadd_odd_exp : std_logic_vector(L_max-1 downto 1);
		begin
			-- at the moment non-generic code
			For K in 1 to ACS_units loop
				writeadd_even_exp(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1) := writeadd_even(k)(log2_cc downto log2_cc-Convert_str_nat(pol_value => L, base => "DEC")+2);
				writeadd_odd_exp(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1) := writeadd_odd(k)(log2_cc downto log2_cc-Convert_str_nat(pol_value => L, base => "DEC")+2);
				
				if unsigned(tr_init_state(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1)) =	unsigned(writeadd_even_exp(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1)) then
					allow_hold_best_even(k) <= '1';
				else
					allow_hold_best_even(k) <= '0';
				end if;
				
				if unsigned(tr_init_state(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1)) = unsigned(writeadd_odd_exp(Convert_str_nat(pol_value => L, base => "DEC")-1 downto 1)) then
					allow_hold_best_odd(k) <= '1';
				else
					allow_hold_best_odd(k) <= '0';
				end if;
			end loop;
		end process filter_states1b;
				
	end generate ifg_spr1b;
	
end generate ifg_spr1;


ifg_spr2: if ncodes>1 generate

  forgen1: For I in 1 to ncodes generate
	
		filter_states_fg0: Process(writeadd_even, writeadd_odd, tr_init_state)
			variable writeadd_even_exp, writeadd_odd_exp : std_logic_vector(L_max-1 downto 1);
		begin
				
			For K in 1 to ACS_units loop
				--  I have to compare L_list(I)-1 downto 1  with log2_cc downto 1?  it could be more than 1
			-- in L_list = 3 or 4 and log2_cc=7!!
				if (L_list(I)-1 > log2_cc) then
					writeadd_even_exp(L_list(I)-1 downto L_list(I)-log2_cc) := writeadd_even(k)(log2_cc downto 1);
					writeadd_odd_exp(L_list(I)-1 downto L_list(I)-log2_cc) := writeadd_odd(k)(log2_cc downto 1);
					-- HERE those bits in binary table have to be shifted too!! 
					writeadd_even_exp(log2_ACS_units+1-L_max+L_list(I) downto 1) := binary_table_acs_even(k-1)(log2_ACS_units+1 downto 1+L_max-L_list(I));
					writeadd_odd_exp(log2_ACS_units+1-L_max+L_list(I) downto 1)  :=  binary_table_acs_odd(k-1)(log2_ACS_units+1 downto 1+L_max-L_list(I));
					--writeadd_even_exp(log2_ACS_units+1-L_max+L_list(I) downto 1) := binary_table_acs_even(k-1)(log2_ACS_units+1-L_max+L_list(I) downto 1);
					--writeadd_odd_exp(log2_ACS_units+1-L_max+L_list(I) downto 1) := binary_table_acs_odd(k-1)(log2_ACS_units+1-L_max+L_list(I) downto 1);
				else
					writeadd_even_exp(L_list(I)-1 downto 1) := writeadd_even(k)(log2_cc downto log2_cc-L_list(I)+2);
					writeadd_odd_exp(L_list(I)-1 downto 1) := writeadd_odd(k)(log2_cc downto log2_cc-L_list(I)+2);
				end if;
			
			
				if unsigned(tr_init_state(L_list(I)-1 downto 1)) = 
					 unsigned(writeadd_even_exp(L_list(I)-1 downto 1)) then
					allow_hold_best_even_premux(I)(k) <= '1';
				else
					allow_hold_best_even_premux(I)(k) <= '0';
				end if;
				if unsigned(tr_init_state(L_list(I)-1 downto 1)) = 
					 unsigned(writeadd_odd_exp(L_list(I)-1 downto 1)) then
					allow_hold_best_odd_premux(I)(k) <= '1';
				else
					allow_hold_best_odd_premux(I)(k) <= '0';
				end if;
			end loop;
		
		end process filter_states_fg0;
		
	end generate forgen1;
	
	ifg_acs_1: if ACS_units=1 generate
		
		ncodes_mux_3 : Process(allow_hold_best_even_premux, sel_code_early)
	
			variable mux_temp_p :	Vector_2D(0 to ncodes);
			variable tmp_and_sel : Std_Logic_Vector(sel_code_early'HIGH downto 0);
			variable mux_addition : Std_Logic; 
	
		begin
			acs_loop: for K in 1 to ACS_units loop
				tmp_and_sel(0) := '1';
				mux_temp_p(0)(K) := '0';
				or_loop: For I in 1 to ncodes loop
					mux_temp_p(I)(K) := '0';
					and_loop: For J in 1 to sel_code_early'HIGH loop
						if binary_table_ncodes(I-1)(J)='0' then -- bit J of I-1 is 0
							tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_early(J);
						else  -- bit J of I-1 is 1
							tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_early(J);
						end if;
					end loop and_loop;
					-- moving to MSB 
					mux_addition := 
						allow_hold_best_even_premux(I)(K) and tmp_and_sel(sel_code_early'HIGH);
					mux_temp_p(I)(K) :=
					mux_temp_p(I-1)(K) or mux_addition;
					end loop or_loop;
				allow_hold_best_even(k) <= mux_temp_p(ncodes)(k);
			end loop acs_loop;	
			
		end process ncodes_mux_3;
	
		ncodes_mux_4 : Process(allow_hold_best_odd_premux, sel_code_later)
	
			variable mux_temp_p :	Vector_2D(0 to ncodes);
			variable tmp_and_sel : Std_Logic_Vector(sel_code_later'HIGH downto 0);
			variable mux_addition : Std_Logic; 
	
		begin
			acs_loop: for K in 1 to ACS_units loop
				tmp_and_sel(0) := '1';
				mux_temp_p(0)(K) := '0';
				or_loop: For I in 1 to ncodes loop
					mux_temp_p(I)(K) := '0';
					and_loop: For J in 1 to sel_code_later'HIGH loop
						if binary_table_ncodes(I-1)(J)='0' then -- bit J of I-1 is 0
							tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_later(J);
						else  -- bit J of I-1 is 1
							tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_later(J);
						end if;
					end loop and_loop;
					-- moving to MSB 
					mux_addition := 
						allow_hold_best_odd_premux(I)(K) and tmp_and_sel(sel_code_later'HIGH);
					mux_temp_p(I)(K) :=
					mux_temp_p(I-1)(K) or mux_addition;
					end loop or_loop;
				allow_hold_best_odd(k) <= mux_temp_p(ncodes)(k);
			end loop acs_loop;	
			
		end process ncodes_mux_4;
	end generate ifg_acs_1;
	
	
	ifg_acs_g: if ACS_units>1 generate
		
		ncodes_mux_4 : Process(allow_hold_best_even_premux, allow_hold_best_odd_premux, sel_code_early, sel_code_later)
	
			variable mux_temp_p_e, mux_temp_p_o :	Vector_2D(0 to ncodes);
			variable tmp_and_sel : Std_Logic_Vector(sel_code_later'HIGH downto 0);
			variable mux_addition_e, mux_addition_o : Std_Logic;
			variable allow_hold_best_even_var, allow_hold_best_odd_var : std_logic_vector(ACS_units downto 1);
	
		begin
			acs_loop: for K in 1 to ACS_units/2 loop
				tmp_and_sel(0) := '1';
				mux_temp_p_e(0)(K) := '0';
				mux_temp_p_o(0)(K) := '0';
				or_loop: For I in 1 to ncodes loop
					mux_temp_p_e(I)(K) := '0';
					mux_temp_p_o(I)(K) := '0';
					and_loop: For J in 1 to sel_code_early'HIGH loop
						if binary_table_ncodes(I-1)(J)='0' then --bit J of I-1 is 0
							tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_early(J);
						else  --bit J of I-1 is 1
							tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_early(J);
						end if;
					end loop and_loop;
					mux_addition_e := 
						allow_hold_best_even_premux(I)(K) and tmp_and_sel(sel_code_early'HIGH);
					mux_addition_o := 
						allow_hold_best_odd_premux(I)(K) and tmp_and_sel(sel_code_early'HIGH);
					mux_temp_p_e(I)(K) := mux_temp_p_e(I-1)(K) or mux_addition_e;
					mux_temp_p_o(I)(K) := mux_temp_p_o(I-1)(K) or mux_addition_o;
				end loop or_loop;
				allow_hold_best_even_var(k) := mux_temp_p_e(ncodes)(k);
				allow_hold_best_odd_var(k)  := mux_temp_p_o(ncodes)(k);
			end loop acs_loop;	
			acs_loop2: for K in ACS_units/2+1 to ACS_units loop
				tmp_and_sel(0) := '1';
				mux_temp_p_e(0)(K) := '0';
				mux_temp_p_o(0)(K) := '0';
				or_loop2: For I in 1 to ncodes loop
					mux_temp_p_e(I)(K) := '0';
					mux_temp_p_o(I)(K) := '0';
					and_loop2: For J in 1 to sel_code_later'HIGH loop
						if binary_table_ncodes(I-1)(J)='0' then --bit J of I-1 is 0
							tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code_later(J);
						else  --bit J of I-1 is 1
							tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code_later(J);
						end if;
					end loop and_loop2;
					mux_addition_e := 
						allow_hold_best_even_premux(I)(K) and tmp_and_sel(sel_code_later'HIGH);
					mux_addition_o := 
						allow_hold_best_odd_premux(I)(K) and tmp_and_sel(sel_code_later'HIGH);
					mux_temp_p_e(I)(K) :=	mux_temp_p_e(I-1)(K) or mux_addition_e;
					mux_temp_p_o(I)(K) :=	mux_temp_p_o(I-1)(K) or mux_addition_o;
				end loop or_loop2;
				allow_hold_best_even_var(k) := mux_temp_p_e(ncodes)(k);
				allow_hold_best_odd_var(k)  := mux_temp_p_o(ncodes)(k);
			end loop acs_loop2;
			allow_hold_best_even <= allow_hold_best_even_var;
			allow_hold_best_odd  <= allow_hold_best_odd_var;
			
		end process ncodes_mux_4;
	end generate ifg_acs_g;

end generate ifg_spr2;


-- any value now
--start_best_find <= survlatch_node;

hold_best: process(clk, reset)

-- SPR 195369  :  Need to modify best state finder behaviour at the last symbol
-- it will have to search in a subset of states devised from tr_init_state.
-- The number of states in that subset is 2 ^ (L_max - L(i))  
-- with just 1 state = tr_init_state when L(i) = L_max. 

  variable hold_data_even_d, hold_data_odd_d : Vector_2D(ACS_units downto 1);
	variable data_even_mux_in, data_odd_mux_in : Vector_2D(ACS_units downto 1);
  variable hold_state_even_d, hold_state_odd_d : Vector_2D(ACS_units downto 1);
	variable update_eve, update_odd : Std_Logic_Vector(ACS_units downto 1);
	variable hold_ge_new_eve, hold_ge_new_odd : Std_Logic_Vector(ACS_units downto 1);

begin
	if reset = '1' then
		For K in 1 to ACS_units loop
			hold_data_even(K)(bmgwide downto 1) <= (others => '0');
			hold_data_odd(K)(bmgwide downto 1) <= (others => '0');
			hold_state_even_q(K)(log2_cc downto 1) <= (others => '0');
			hold_state_odd_q(K)(log2_cc downto 1) <= (others => '0');
		end loop;
	elsif Rising_edge(clk) then

			-- I need the mux on eop_even_round here!!
			-- eop_later_round_ind is a 1 clock delay of eop_early_round_ind
			-- it has to be applied to even and odd when acs>1!! Do Correct
		if ACS_units=1 then
			data_even_mux_in(1)(bmgwide downto 1) := 
					(data_even_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => not eop_early_round_ind)) or
			    (data_even_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => allow_hold_best_even(1))) or
					(vector_zero                           and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => not allow_hold_best_even(1)));
			data_odd_mux_in(1)(bmgwide downto 1) := 
					(data_odd_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => not eop_later_round_ind)) or
			    (data_odd_pre_in(1)(bmgwide downto 1) and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => allow_hold_best_odd(1))) or
					(vector_zero                           and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => not allow_hold_best_odd(1)));
		else
			For K in 1 to ACS_units/2 loop
				data_even_mux_in(K)(bmgwide downto 1) := 
						(data_even_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not eop_early_round_ind)) or
						(data_even_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => allow_hold_best_even(k))) or
						(vector_zero                           and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => not allow_hold_best_even(k)));
				data_odd_mux_in(K)(bmgwide downto 1) := 
						(data_odd_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not eop_early_round_ind)) or
						(data_odd_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => allow_hold_best_odd(k))) or
						(vector_zero                           and (bmgwide downto 1 => eop_early_round_ind) and (bmgwide downto 1 => not allow_hold_best_odd(k)));
			end loop;
			For K in ACS_units/2+1 to ACS_units loop
				data_even_mux_in(K)(bmgwide downto 1) := 
						(data_even_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not eop_later_round_ind)) or
						(data_even_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => allow_hold_best_even(k))) or
						(vector_zero                           and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => not allow_hold_best_even(k)));
				data_odd_mux_in(K)(bmgwide downto 1) := 
						(data_odd_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not eop_later_round_ind)) or
						(data_odd_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => allow_hold_best_odd(k))) or
						(vector_zero                           and (bmgwide downto 1 => eop_later_round_ind) and (bmgwide downto 1 => not allow_hold_best_odd(k)));
			end loop;
		end if;
		For K in 1 to ACS_units loop
			if unsigned(hold_data_even(k)(bmgwide downto 1)) > 
			   unsigned(data_even_mux_in(k)(bmgwide downto 1)) then
			  hold_ge_new_eve(k) := '1';
			else
			  hold_ge_new_eve(k) := '0';
			end if;
			if unsigned(hold_data_odd(k)(bmgwide downto 1)) > 
			   unsigned(data_odd_mux_in(k)(bmgwide downto 1)) then
			  hold_ge_new_odd(k) := '1';
			else
			  hold_ge_new_odd(k) := '0';
			end if;
			-- how do I deal with this in eop search mode? 
			-- if it is one of the end states we are looking just update, otherwise update to zero!
			-- that means I need an extra mux around data_even_pre_in
			update_eve(k) := hold_ge_new_eve(k) and not start_find_best_even(k);
			update_odd(k) := hold_ge_new_odd(k) and not start_find_best_odd(k);
			hold_data_even_d(K)(bmgwide downto 1) := 
					(hold_data_even(k)(bmgwide downto 1) and (bmgwide downto 1 => update_eve(k))) or
					(data_even_mux_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not update_eve(k)));
			hold_data_odd_d(K)(bmgwide downto 1) := 
					(hold_data_odd(k)(bmgwide downto 1) and (bmgwide downto 1 => update_odd(k))) or
			    (data_odd_pre_in(k)(bmgwide downto 1) and (bmgwide downto 1 => not update_odd(k)));
      -- for the state 
			hold_state_even_d(K)(log2_cc downto 1) := 
			    (hold_state_even_q(k)(log2_cc downto 1) and (log2_cc downto 1 => update_eve(k))) or
			    (writeadd_even(k)(log2_cc downto 1) and (log2_cc downto 1 => not update_eve(k)));
			hold_state_odd_d(K)(log2_cc downto 1) := 
			    (hold_state_odd_q(k)(log2_cc downto 1) and (log2_cc downto 1 => update_odd(k))) or
			    (writeadd_odd(k)(log2_cc downto 1) and (log2_cc downto 1 => not update_odd(k)));
			-- latching
		
		end loop;
		-- if ACS_units=1 then
			-- hold_state_even_q(1)(log2_cc downto 1) <= hold_state_even_d(1)(log2_cc downto 1);
			-- hold_data_even(1)(bmgwide downto 1) <= hold_data_even_d(1)(bmgwide downto 1);
			-- hold_state_odd_q(1)(log2_cc downto 1) <= hold_state_odd_d(1)(log2_cc downto 1);
			-- hold_data_odd(1)(bmgwide downto 1) <= hold_data_odd_d(1)(bmgwide downto 1);
		-- else
			--for K in 1 to ACS_units/2 loop
			for K in 1 to ACS_units loop
				hold_state_even_q(K)(log2_cc downto 1) <= hold_state_even_d(K)(log2_cc downto 1);
				hold_state_odd_q(K)(log2_cc downto 1) <= hold_state_odd_d(K)(log2_cc downto 1);
				hold_data_even(K)(bmgwide downto 1) <= hold_data_even_d(K)(bmgwide downto 1);
				hold_data_odd(K)(bmgwide downto 1) <= hold_data_odd_d(K)(bmgwide downto 1);
			end loop;
			--for K in ACS_units/2+1 to ACS_units loop
				--hold_state_even_q(K)(log2_cc downto 1) <= hold_state_even_d(K)(log2_cc downto 1);
				--hold_state_odd_q(K)(log2_cc downto 1) <= hold_state_odd_d(K)(log2_cc downto 1);
				--hold_data_even(K)(bmgwide downto 1) <= hold_data_even_d(K)(bmgwide downto 1);
				--hold_data_odd(K)(bmgwide downto 1) <= hold_data_odd_d(K)(bmgwide downto 1);
			--end loop;
		--end if;

	end if;

end process hold_best;


--   generic piramid of comparators and holders

fg7c : If ACS_units > 1 generate
	fg7c1 : for k in 1 to ACS_units/2 generate

	-- for ACS=4, data_even/odd 1 and 2 have to go to bmgsel 1, 3, 5, 7 in
	-- order to end up in bmgsel(2)(1)(..) hence being held by
	-- bmgsel(2)(3)(..) at the tip. (Alex 18/03/2003)

		bmgsel_d(0)(K)(bmgwide downto 1) <= hold_data_even(K)(bmgwide downto 1);
		bmgsel_d(0)(K+ACS_units/2)(bmgwide downto 1) <= hold_data_odd(K)(bmgwide downto 1);
		--binary_table : log2_ACS_units
		--bmgaddhold_d(K)(L_max-1 downto log2_cc) <= binary_table(k-1)(log2_ACS_units+1 downto 1);
		bmgaddsel_d(0)(K)(log2_ACS_units+1 downto 1) <= binary_table(2*k-2)(log2_ACS_units+1 downto 1);
		-- L=7 , ACS=4 => log2_cc = 3
		bmgaddsel_d(0)(K)(L_max-1 downto log2_ACS_units+2) <= hold_state_even_q(K)(log2_cc downto 1);
		
		bmgaddsel_d(0)(K+ACS_units/2)(log2_ACS_units+1 downto 1) <= binary_table(2*k-1)(log2_ACS_units+1 downto 1);
		bmgaddsel_d(0)(K+ACS_units/2)(L_max-1 downto log2_ACS_units+2) <= hold_state_odd_q(K)(log2_cc downto 1);
    
	end generate fg7c1;	

	fg7c2 : for k in ACS_units/2+1 to ACS_units generate

	-- for ACS=4, data_even/odd 1 and 2 have to go to bmgsel 1, 3, 5, 7 in
	-- order to end up in bmgsel(2)(1)(..) hence being held by
	-- bmgsel(2)(3)(..) at the tip. (Alex 18/03/2003)

		bmgsel_d(0)(K+ACS_units/2)(bmgwide downto 1) <= hold_data_even(K)(bmgwide downto 1);
		bmgsel_d(0)(K+ACS_units)(bmgwide downto 1)   <= hold_data_odd(K)(bmgwide downto 1);
		--binary_table : log2_ACS_units
		--bmgaddhold_d(K)(L_max-1 downto log2_cc) <= binary_table(k-1)(log2_ACS_units+1 downto 1);
		bmgaddsel_d(0)(K+ACS_units/2)(log2_ACS_units+1 downto 1) <= binary_table(2*k-2)(log2_ACS_units+1 downto 1);
		-- L=7 , ACS=4 => log2_cc = 3
		bmgaddsel_d(0)(K+ACS_units/2)(L_max-1 downto log2_ACS_units+2) <= hold_state_even_q(K)(log2_cc downto 1);
		
		bmgaddsel_d(0)(K+ACS_units)(log2_ACS_units+1 downto 1) <= binary_table(2*k-1)(log2_ACS_units+1 downto 1);
		bmgaddsel_d(0)(K+ACS_units)(L_max-1 downto log2_ACS_units+2) <= hold_state_odd_q(K)(log2_cc downto 1);

	end generate fg7c2;
	-- more re-arrenging
	fg7c3 : for k in 1 to ACS_units generate
		bmgsel_q(0)(2*K-1)(bmgwide downto 1)  <= bmgsel_d(0)(K)(bmgwide downto 1);
		bmgsel_q(0)(2*K  )(bmgwide downto 1)  <= bmgsel_d(0)(K+ACS_units)(bmgwide downto 1);
		bmgaddsel_q(0)(2*K-1)(L_max-1 downto 1)  <= bmgaddsel_d(0)(K)(L_max-1 downto 1);
		bmgaddsel_q(0)(2*K  )(L_max-1 downto 1)  <= bmgaddsel_d(0)(K+ACS_units)(L_max-1 downto 1);
	end generate fg7c3;
	  	
end generate fg7c;

fg7d : If ACS_units = 1 generate

		bmgsel_q(0)(1)(bmgwide downto 1) <= hold_data_even(1)(bmgwide downto 1);
		bmgsel_q(0)(2)(bmgwide downto 1) <= hold_data_odd(1)(bmgwide downto 1);
		bmgaddsel_q(0)(1)(1 downto 1) <= "0";
		bmgaddsel_q(0)(1)(L_max-1 downto 2) <= hold_state_even_q(1)(log2_cc downto 1);
		bmgaddsel_q(0)(2)(1 downto 1) <= "1";
		bmgaddsel_q(0)(2)(L_max-1 downto 2) <= hold_state_odd_q(1)(log2_cc downto 1);
    
--		bmgsel_q(0)(1)(bmgwide downto 1)  <= bmgsel_d(0)(1)(bmgwide downto 1);
--		bmgsel_q(0)(2)(bmgwide downto 1)  <= bmgsel_d(0)(2)(bmgwide downto 1);
--		bmgaddsel_q(0)(1)(L_max-1 downto 1)  <= bmgaddsel_d(0)(1)(L_max-1 downto 1);
--		bmgaddsel_q(0)(2)(L_max-1 downto 1)  <= bmgaddsel_d(0)(2)(L_max-1 downto 1);

end generate fg7d;



ifg5b: if log2_ACS_units > 0 generate -- equivalent to ACS_units > 1
	fg2d: for I in 0 to log2_ACS_units-1 generate
		fg3d: for J in 1 to tree_arch(I) generate
	    comp3: process(bmgsel_q) 
	    begin
	    	if unsigned(bmgsel_q(I)(J)(bmgwide downto 1)) < unsigned(bmgsel_q(I)(J+tree_arch(I))(bmgwide downto 1)) then
	    		sel_tree(I)(J) <= '0';
	    	else
	    		sel_tree(I)(J) <= '1';
	    	end if;
	    end process comp3;
	  	bmgsel_d(I+1)(J)(bmgwide downto 1) <= 
	  	(bmgsel_q(I)(J)(bmgwide downto 1) and (bmgwide downto 1 => sel_tree(I)(J))) or
	  	(bmgsel_q(I)(J+tree_arch(I))(bmgwide downto 1) and (bmgwide downto 1 => not sel_tree(I)(J)));
	  	bmgaddsel_d(I+1)(J)(L_max-1 downto 1) <= 
	  	(bmgaddsel_q(I)(J)(L_max-1 downto 1) and (L_max-1 downto 1 => sel_tree(I)(J))) or
	  	(bmgaddsel_q(I)(J+tree_arch(I))(L_max-1 downto 1) and (L_max-1 downto 1 => not sel_tree(I)(J)));

	    -- if I remove the enable from here I have to make sure that 
			-- in case acs gets stopped the best metric and its state are
			-- kept 
	    bmgsel : Process(clk, reset)
	    begin
	    if reset='1' then
	      --bmgsel_q(I+1)(J)(bmgwide downto 1) <= (others => '0');
				--bmgaddsel_q(I+1)(J)(L_max-1 downto 1) <= (others => '0');
				-- For readability
				bmgsel_q(I+1)(J) <= (others => '0');
				bmgaddsel_q(I+1)(J) <= (others => '0');
	    elsif Rising_edge(clk) then
        bmgsel_q(I+1)(J)(bmgwide downto 1) <= bmgsel_d(I+1)(J)(bmgwide downto 1);
				bmgaddsel_q(I+1)(J)(L_max-1 downto 1) <= bmgaddsel_d(I+1)(J)(L_max-1 downto 1);
	    end if;
	    end process bmgsel;          

		end generate fg3d;
	end generate fg2d;
end generate ifg5b;

-- need to hold bmgsel_q()(1)() and bmgaddsel_q()(1)() before comparing with ()(2) 
-- I use ()(3) to hold ()(1)


-- tip of pyramid comparator and selector
-- susbtitute I by log2_ACS_units and J by 1 
comp_tip: process(bmgsel_q(log2_ACS_units)) 
begin
	if unsigned(bmgsel_q(log2_ACS_units)(3)(bmgwide downto 1)) < unsigned(bmgsel_q(log2_ACS_units)(2)(bmgwide downto 1)) then
		sel_tree(log2_ACS_units)(1) <= '0';
	else
		sel_tree(log2_ACS_units)(1) <= '1';
	end if;
end process comp_tip;
bmgsel_d(log2_ACS_units+1)(1)(bmgwide downto 1) <= 
(bmgsel_q(log2_ACS_units)(3)(bmgwide downto 1) and (bmgwide downto 1 => sel_tree(log2_ACS_units)(1))) or
(bmgsel_q(log2_ACS_units)(2)(bmgwide downto 1) and (bmgwide downto 1 => not sel_tree(log2_ACS_units)(1)));
bmgaddsel_d(log2_ACS_units+1)(1)(L_max-1 downto 1) <= 
(bmgaddsel_q(log2_ACS_units)(3)(L_max-1 downto 1) and (L_max-1 downto 1 => sel_tree(log2_ACS_units)(1))) or
(bmgaddsel_q(log2_ACS_units)(2)(L_max-1 downto 1) and (L_max-1 downto 1 => not sel_tree(log2_ACS_units)(1)));

bmgsel_tip : Process(clk, reset)
begin
if reset='1' then
  bmgsel_q(log2_ACS_units+1)(1)(bmgwide downto 1) <= (others => '0');
	bmgaddsel_q(log2_ACS_units+1)(1)(L_max-1 downto 1) <= (others => '0');
	bmgsel_q(log2_ACS_units)(3)(bmgwide downto 1) <= (others => '0');
	bmgaddsel_q(log2_ACS_units)(3)(L_max-1 downto 1) <= (others => '0');
elsif Rising_edge(clk) then
	bmgsel_q(log2_ACS_units)(3)(bmgwide downto 1) <= bmgsel_q(log2_ACS_units)(1)(bmgwide downto 1);
	bmgaddsel_q(log2_ACS_units)(3)(L_max-1 downto 1) <= bmgaddsel_q(log2_ACS_units)(1)(L_max-1 downto 1);
	if latch_best_q='1' then
--	if latch_best='1' then
	-- this latch_best still needs refinement and re-test for different ACS and L
		bmgsel_q(log2_ACS_units+1)(1)(bmgwide downto 1) <= bmgsel_d(log2_ACS_units+1)(1)(bmgwide downto 1);
		bmgaddsel_q(log2_ACS_units+1)(1)(L_max-1 downto 1) <= bmgaddsel_d(log2_ACS_units+1)(1)(L_max-1 downto 1);
	end if;

end if;
end process bmgsel_tip;


-- latch_best is been develop with L=7, ACS=4, will serve for other combinations? 
-- I hope so with "latch_best_point"

-- I need to clarify this: For L=7 ACS=4 I need butsecond signal
-- but for L=5 ACS=1 nofirst suffices.

ifg999: if ACS_units=1 and log2_cc=3 generate

	round_clk3 : process(clk, reset)
	begin
		if reset='1' then
			latch_best_q <= '0';
	  elsif Rising_edge(clk) then
	    latch_best_q <= latch_best_in and nofirst;
	  end if;
	end process round_clk3;

end generate ifg999;

ifg99: if log2_cc=3 generate

	round_clk : process(clk, reset)
	begin
		if reset='1' then
	    we_ram_sur <= '0';
			we_ram_acs <= '1';
			init_acs <= '1';
--			latch_best_q <= '0';
	  elsif Rising_edge(clk) then
--	    latch_best_q <= latch_best_in and nofirst;
	    if end_wr_round='1' then
	     	we_ram_sur <= we_ram;
				we_ram_acs <= we_ram;
				init_acs <= acs_init; -- or init_acs_first;
	    end if;
	  end if;
	end process round_clk;

end generate ifg99;

ifg888: if ACS_units>1 or log2_cc>3 generate

  round_clk2 : process(clk, reset)
	begin
		if reset='1' then
			latch_best_q <= '0';
	  elsif Rising_edge(clk) then
	    latch_best_q <= latch_best_in and butsecond;
	  end if;
	end process round_clk2;

end generate ifg888;

ifg88: if log2_cc>3 generate

  round_clk : process(clk, reset)
	begin
		if reset='1' then
	    we_ram_sur <= '0';
			we_ram_acs <= '1';
			init_acs <= '0';
			init_acs_first <= '1';
			we_ram_acs_first <= '1';
--			latch_best_q <= '0';
	  elsif Rising_edge(clk) then
--	    latch_best_q <= latch_best_in and butsecond;
	    if end_wr_round='1' then
	     	we_ram_sur <= we_ram;
				we_ram_acs <= we_ram or we_ram_acs_first;
				init_acs_first <= '0';
				we_ram_acs_first <= '0';
				-- for L=7, ACS=1 at the beginning it has to last throught 2 end_wr_round. 
				-- how do I do this?
				init_acs <= acs_init or init_acs_first;
	    end if;
	  end if;
	end process round_clk;

end generate ifg88;

no_first: process(clk, reset)

begin
	if reset='1' then
		nofirst <='0';
		butsecond <='0';
	elsif Rising_edge(clk) then
		if latch_best_in='1' then
            nofirst <= '1';
            butsecond <= nofirst;
        end if;
	end if;
end process no_first;

--latch_best <= latch_best_q;

--latch_best <= latch_best_in;  -- First filtern then allow_best_latch

-- bestmet internal signal for debugging only.
bestmet <= bmgsel_q(log2_ACS_units+1)(1)(bmgwide downto 1);
--holdaddout_d <= bmgaddsel_q(log2_ACS_units+1)(1)(L_max-1 downto 1);
bestadd <= bmgaddsel_d(log2_ACS_units+1)(1)(L_max-1 downto 1);

-- logic for acs initialization
fg7f : for k in 1 to ACS_units generate

	--binary_table : log2_ACS_units
	state_number(K)(log2_ACS_units+1 downto 1) <= binary_table(2*k-2)(log2_ACS_units+1 downto 1);
	-- L=7 , ACS=4 => log2_cc = 3
	state_number(K)(L_max-1 downto log2_ACS_units+2) <= writeadd_even(K)(log2_cc downto 1);
	
	state_number(K+ACS_units)(log2_ACS_units+1 downto 1) <= binary_table(2*k-1)(log2_ACS_units+1 downto 1);
	state_number(K+ACS_units)(L_max-1 downto log2_ACS_units+2) <= writeadd_odd(K)(log2_cc downto 1);

end generate fg7f;	

if_mux : if ncodes > 1 generate  

  ncodes_mux_1 : Process(state_number, sel_code)

    variable state_number_mux_p :	Vector_3D(0 to ncodes);
    variable tmp_and_sel : Std_Logic_Vector(sel_code'HIGH downto 0);
		variable mux_addition : Std_Logic_Vector(L_max-1 downto 1);

  begin
    acs_loop: for K in 1 to 2*ACS_units loop
      tmp_and_sel(0) := '1';
      state_number_mux_p(0)(K)(L_max-1 downto 1) := (others => '0');
			or_loop: For I in 1 to ncodes loop
			  state_number_mux_p(I)(K)(L_max-1 downto 1) := (others => '0');
				-- this is for readability
				state_number_mux_p(I)(K)(BMG_MAX downto L_max) := (others => '0');
				-- end readability
      	and_loop: For J in 1 to sel_code'HIGH loop
      		if binary_table_ncodes(I-1)(J)='0' then -- bit J of I-1 is 0
      			tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code(J);
      		else  -- bit J of I-1 is 1
      			tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code(J);
      		end if;
      	end loop and_loop;
				-- moving to MSB 
				mux_addition(L_max-1 downto L_max-L_list(I)+1) := 
				  state_number(K)(L_max-1 downto L_max-L_list(I)+1) and (L_max-1 downto L_max-L_list(I)+1 => tmp_and_sel(sel_code'HIGH));
				if L_max > L_list(I) then
					mux_addition(L_max-L_list(I) downto 1) := (others => '0');
				end if;
				state_number_mux_p(I)(K)(L_max-1 downto 1) :=
      	state_number_mux_p(I-1)(K)(L_max-1 downto 1) or mux_addition;
--      	state_number(K)(L_max-1 downto L_max-L_list(I)+1) and (L_max-1 downto L_max-L_list(I)+1 => tmp_and_sel(sel_code'HIGH)));
      end loop or_loop;
    end loop acs_loop;	
    state_number_mux <= state_number_mux_p(ncodes);
  end process ncodes_mux_1;

  ncodes_mux_2 : Process(bm_init_state, sel_code)

    variable bm_init_state_mux_p :	Vector_2D(0 to ncodes);
    variable tmp_and_sel : Std_Logic_Vector(sel_code'HIGH downto 0);
		variable mux_addition : Std_Logic_Vector(L_max-1 downto 1);

  begin
    tmp_and_sel(0) := '1';
    bm_init_state_mux_p(0)(L_max-1 downto 1) := (others => '0');
		or_loop: For I in 1 to ncodes loop
    	bm_init_state_mux_p(I)(L_max-1 downto 1) := (others => '0');
    	and_loop: For J in 1 to sel_code'HIGH loop
    		if binary_table_ncodes(I-1)(J)='0' then -- bit J of I-1 is 0
    			tmp_and_sel(J) := tmp_and_sel(J-1) and not sel_code(J);
    		else  -- bit J of I-1 is 1
    			tmp_and_sel(J) := tmp_and_sel(J-1) and sel_code(J);
    		end if;
    	end loop and_loop;
			-- moving to MSB
    	mux_addition(L_max-1 downto L_max-L_list(I)+1) := 
			  bm_init_state(L_max-1 downto L_max-L_list(I)+1) and (L_max-1 downto L_max-L_list(I)+1 => tmp_and_sel(sel_code'HIGH));
			if L_max > L_list(I) then
				mux_addition(L_max-L_list(I) downto 1) := (others => '0');
			end if;
    	bm_init_state_mux_p(I)(L_max-1 downto 1) :=
    	bm_init_state_mux_p(I-1)(L_max-1 downto 1) or mux_addition;
--    	bm_init_state(L_max-1 downto L_max-L_list(I)+1) and (L_max-1 downto L_max-L_list(I)+1 => tmp_and_sel(sel_code'HIGH)));
    end loop or_loop;

    bm_init_state_mux <= bm_init_state_mux_p(ncodes)(L_max-1 downto 1);
  end process ncodes_mux_2;

end generate if_mux;

if_no_mux : if ncodes=1 generate

  state_number_mux <= state_number;
	bm_init_state_mux <= bm_init_state;

end generate if_no_mux;


-- with multiple constraint lenght : I am going to initilize just state 0
-- see what happens
comps: process(state_number_mux, bm_init_state_mux)

begin
	for I in 1 to ACS_units loop
		if unsigned(state_number_mux(I)(L_max-1 downto 1))=unsigned(bm_init_state_mux) then
			match_even(I) <= '1';
		else
			match_even(I) <= '0';
		end if;
		if unsigned(state_number_mux(I+ACS_units)(L_max-1 downto 1))=unsigned(bm_init_state_mux) then
			match_odd(I) <= '1';
		else
			match_odd(I) <= '0';
		end if;
	end loop;
end process comps;


norm_cnt : Process(clk, reset)
begin
if reset='1' then
  normalizations_int <= (others => '0');
elsif Rising_edge(clk) then
  if normctl_q='1' and acs_end_round='1' and we_ram_acs='1' then
    normalizations_int <= unsigned(normalizations_int) + natural(1);
  end if;
end if;
end process norm_cnt;          

	


--***************
--*** Outputs ***
--***************

latch_best_mark <= latch_best_q and allow_best_latch;
--survlatch <= survlatch_node;
survivetop <= selecttop;
survivebot <= selectbot;
normalizations <= normalizations_int;


--*******************
--*** Readability ***
--*******************

fg_read: For I in 1 to ACS_units generate
if_Read1 : if BMG_MAX > bmgwide generate

	oddmem(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
	evenmem(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
	hypo_nil_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
	hypo_one_sel_q(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
  writeadd_odd(I)(BMG_MAX downto seq_width+1) <= (others => '0');
  writeadd_even(I)(BMG_MAX downto seq_width+1) <= (others => '0');
	bmgsel_q(0)(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
	bmgsel_q(0)(I+ACS_units)(BMG_MAX downto bmgwide+1) <= (others => '0');
	bmgaddsel_q(0)(I)(BMG_MAX downto L_max) <= (others => '0');
	bmgaddsel_q(0)(I+ACS_units)(BMG_MAX downto L_max) <= (others => '0');
	data_odd_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
	data_even_in(I)(BMG_MAX downto bmgwide+1) <= (others => '0');
--	state_vitenca(I)(BMG_MAX downto L_max+1) <= (others => '0');
end generate if_read1;

end generate fg_read;

fg_read2: For I in 1 to 2*ACS_units generate
  if_Read2 : if BMG_MAX > L_max generate
		
		state_number(I)(BMG_MAX downto L_max) <= (others => '0');
  end generate if_read2;
end generate fg_read2;

end architecture rtl;

