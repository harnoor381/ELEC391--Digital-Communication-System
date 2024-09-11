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
-- $RCSfile: auk_vit_par_top_atl_arc_rtl.vhd,v $
-- $Source: /cvs/uksw/dsp_cores/Viterbi/Units/Parallel/atlantic/auk_vit_par_top_atl_arc_rtl.vhd,v $
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

use work.vi_interface.all;
use work.vi_functions.all;


Entity auk_vit_par_top_atl is
    Generic (
                n                     : STRING  := "2"; --"2_2_3_4";
                L                     : STRING  := "7"; --"3_5_5_5";
                dec_mode            : STRING  := "V"; --"V_V_V_T";
                ncodes                 : NATURAL := 1; --4;
                n_max                 : NATURAL := 2; --4;
                log2_n_max          : NATURAL := 1;
                rr_size                : NATURAL := 8; --16;
                constraint_length_m_1 : NATURAL := 8; --4;
                v                     : NATURAL := 54; --30;
                 softbits             : NATURAL := 4;
                bmgwide             : NATURAL := 9; --10;
                vlog_wide             : NATURAL := 6;
                sel_code_size         : NATURAL := 1;
                numerr_size          : NATURAL := 12; -- depens on how big the block is going to be
                size_bus            : NATURAL := 16; -- 16 or 32
                ga                     : STRING := "91 91";
                gb                     : STRING := "121 101";
                gc                     : STRING := "0 125";
                gd                     : STRING := "0 0";
                ge                     : STRING := "0 0";
                gf                     : STRING := "0 0";
                gg                     : STRING := "0 0";
--                punctured_rate : STRING := "unpunctured"; --"2/3";
--                puncturing_pattern : STRING := "00000000";
--                traceback_type : STRING := "Memory";  -- or "Memory"
                parallel_optimization : STRING := "none";  -- none, continuous or block
                best_state_finder     : STRING := "used";
                ber                 : STRING := "used";
                ini_filename        : STRING  := "filename.hex";
                use_altera_syncram  : NATURAL := 1;
                node_sync             : STRING := "unused"
    );
    Port (
        clk : in std_logic; 
        reset : in Std_Logic;
        sink_rdy        : out Std_Logic; 
        sink_val        : in  Std_Logic; 
        sink_sop        : in  Std_Logic; 
        sink_eop        : in  Std_Logic; 
---
        rr : in Std_Logic_Vector(rr_size downto 1);
        eras_sym : in Std_Logic_Vector(n_max downto 1);

        state_node_sync : in Std_Logic_Vector(log2_n_max downto 1);
        sel_code : in Std_Logic_Vector(sel_code_size downto 1);
        -- Alex 18-01-2007  the 2 signals below also have to go for continuous optimization
        tr_init_state : in Std_Logic_Vector(constraint_length_m_1 downto 1);
        tb_type : in Std_Logic;
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);
        ber_clear : in std_logic;

        -- Slave source side control signals
        --  Alex 18-01-2007  I have to remove the _slave suffix 
        source_rdy : in Std_Logic; 
        source_val : out Std_Logic; 
        source_sop : out Std_Logic; 
        source_eop : out Std_Logic; 
        -- a single bit for atlantic for output data
        decbit : out Std_Logic;

        bestmet : out Std_Logic_Vector(bmgwide downto 1);
        bestadd : out Std_Logic_Vector(constraint_length_m_1 downto 1);
        normalizations : out Std_Logic_Vector(8 downto 1);
         numerr : out Std_Logic_Vector(numerr_size downto 1)
    );

    attribute altera_attribute : string;
    attribute altera_attribute of auk_vit_par_top_atl : entity is "-name MESSAGE_DISABLE 14130";
    
end entity auk_vit_par_top_atl;    


Architecture rtl of auk_vit_par_top_atl is


-- Component instantiated by Turbo autoplace on 03/08/2003 at 17:31:38
COMPONENT auk_vit_par_bmp_atl
    generic (
        n                     : STRING  := "2_2_3_4";
        L                     : STRING  := "3_5_5_5";
        modes                : STRING  := "V_V_V_T";
        ncodes : NATURAL := 2;
        n_max : NATURAL := 2;
        log2_n_max : natural := 1;
        L_max : NATURAL := 7;
        softbits : NATURAL := 4;
        bmgwide : NATURAL := 12;
        opt_par : STRING := "None";  -- none, continuous or block
        best_state_finder : STRING := "used";
        BER : STRING := "used";
        node_sync : STRING := "unused";
--        config : STRING := "std_no_max_info";
  -- possible others : std_no_max_info, ptcm_no_max_info, std_max_info and ptcm_max_info
        ga : STRING := "19";
        gb : STRING := "29";
        gc : STRING := "0";
        gd : STRING := "0";
        ge : STRING := "0";
        gf : STRING := "0";
        gg : STRING := "0"
        );        
    port (
        clk : in std_logic; 
        reset : in Std_Logic;
        enable : in Std_Logic;

        ena_master_sink : out Std_Logic; -- output in Master mode, input in Slave mode
        val_sink : in Std_Logic; -- only used in Master Sink
        sop_sink : in Std_Logic; -- regardless of Master or Slave mode
        eop_sink : in Std_Logic; -- regardless of Master or Slave mode
        allow_ena_assert_from_trb : in Std_Logic;

        state_node_sync : in Std_Logic_Vector(log2_n_max downto 1);
         sel_code : in Std_Logic_Vector(LOG2_ceil_avoid_one(ncodes) downto 1);
        tr_init_state : in Std_Logic_Vector(L_max-1 downto 1);
        data_for_BER : out Std_Logic_Vector(2*n_max downto 1);
        rr : in Std_Logic_Vector(n_max*softbits downto 1);
        eras_sym : in Std_Logic_Vector(n_max downto 1);
        --
        rrff : in std_logic_matrix(n_max downto 1, softbits+1 downto 1);
        survready, baddready : out Std_Logic;
        normalizations : out Std_Logic_Vector(8 downto 1);
         survive : out Std_Logic_Vector(2**(L_max-1) downto 1);
        bestadd : out Std_Logic_Vector(L_max-1 downto 1);
        bestmet : out Std_Logic_Vector(bmgwide downto 1);
        sink_eop_del : out Std_Logic
      );
END COMPONENT;

  

--COMPONENT auk_vit_par_pun_atl
--    Generic (
--                n : NATURAL := 2;
--                 softbits : NATURAL := 4;
--                punctured_rate : STRING := "2/3";
--                puncturing_pattern : STRING := "00100001"
--    );
--    Port (
--        clk, reset, enable_in : in Std_Logic;
--         rr : in Std_Logic_Vector(softbits downto 1);
--        enable_out : out Std_Logic;
--        eras_sym : out Std_Logic_Vector(n downto 1);
--         yy : out Std_Logic_Vector(n*softbits downto 1)
--    );    
--END COMPONENT;


-- Component instantiated by Turbo autoplace on 03/08/2003 at 17:42:42
COMPONENT auk_vit_par_trb_atl
    Generic (
        L : NATURAL := 9;
        v : NATURAL := 50;
        ncodes : NATURAL := 1;
        vlog_wide : NATURAL := 6;
        -- config : STRING := "std_no_max_info";
        ini_filename        : STRING  := "filename.hex";
        use_altera_syncram  : NATURAL := 1;
        dev_family : STRING := "Stratix"
    );
    Port (
        clk   : in std_logic; 
        reset : in  std_logic;
        enable : in std_logic;
        survready, baddready : in Std_Logic;
        --ena_slave_source, 
        sink_sop_q : in Std_Logic; 
        sink_eop_del : in Std_Logic;
        allow_ena_assert : out Std_Logic;
        data_available   : out std_logic;
        --val_source, 
        sop_source, eop_source : out Std_Logic; 
        survive : in Std_Logic_Vector(2**(L-1) downto 1);
        tb_length     : in Std_Logic_Vector(vlog_wide downto 1);
        tr_init_state : in Std_Logic_Vector(L-1 downto 1);
        tb_type : in Std_Logic;
        bestadd : in Std_Logic_Vector(L-1 downto 1);
         decbit : out Std_Logic
    );    
END COMPONENT;



-- Component instantiated by Turbo autoplace on 06/08/2003 at 15:18:22
COMPONENT auk_vit_par_ber_atl
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
END COMPONENT;

-- Alex 23-01-2007  this component will have to be moved to library.
Component auk_dspip_avalon_str_ctrl
    Generic (
                ready_latency : natural := 1
    );
        Port (
            clk                          : in std_logic;
            reset                        : in std_logic;
            ----------------- DESIGN SIDE SOURCE SIGNALS
            data_available    : in std_logic;
            sop_source_int    : in std_logic;
            eop_source_int    : in std_logic;
            enable_source     : out std_logic;
            ----------------- AVALON_STREAMING SOURCE SIDE SIGNALS 
            source_ready        : in std_logic;
            source_val            : out std_logic;
            source_sop            : out std_logic;
            source_eop            : out std_logic
    );
end Component;


-- For block optimization vlog_wide has to be: 
--Constant vlog_wide : NATURAL := LOG2_ceil_table(v+L+3);  -- to cope with max -> v+L+3

-- For the time being not a top level parameter
-- constant config : STRING := "std_no_max_info";
-- To activate std_max_info also it is necessary to go to file auk_vit_par_trb_atl_arc_mem.vhd and
-- and change the start of stoadd

Constant ready_latency : natural := 1;

--constant config : STRING := "std_max_info";

Constant L_max : NATURAL := constraint_length_m_1+1;
Constant GND : Std_Logic := '0';
constant maxstates : NATURAL := 2**(L_max-1);
Constant n_list : NATURAL_ARRAY(1 to ncodes) := Get_n_list(n => n, ncodes => ncodes);
Constant n_min : NATURAL := Get_n_min(n_list);
Constant family : STRING := "Stratix";

--Constant n_max_tcm : NATURAL := Get_n_max_modes(n => n, m => modes);


signal rrff_d, rrff_q : std_logic_matrix(n_max downto 1, softbits+1 downto 1);
--Signal rr_2_bmp : Std_Logic_Vector(n_max*(softbits+1) downto 1);
Signal survive : Std_Logic_Vector(maxstates downto 1);
Signal pun_enabling : Std_Logic;
signal decbitnode_mem, decbitnode_cnt, decbitnode_blk, decbitnode : Std_Logic;
signal outvalidnode : Std_Logic;
--Signal eras_sym_int : Std_Logic_Vector(n_max downto 1);


signal data_for_BER : Std_Logic_Vector(2*n_max downto 1);
signal survready, baddready : Std_Logic;
signal bestadd_int : Std_Logic_Vector(L_max-1 downto 1);
Signal allow_ena_assert : Std_Logic;
Signal sink_sop_q, source_sop_node, source_eop_node, source_val_node : Std_Logic;
signal sink_eop_del : Std_Logic;
signal tb_length_q : Std_Logic_Vector(vlog_wide downto 1);
Signal state_node_sync_q : Std_Logic_Vector(log2_n_max downto 1);
Signal sel_code_q : Std_Logic_Vector(sel_code_size downto 1);
Signal tr_init_state_q : Std_Logic_Vector(constraint_length_m_1 downto 1);
Signal tb_type_q : Std_Logic;
Signal source_val_cnt, source_val_mem : std_logic;
Signal allow_ena_assert_mem, allow_ena_assert_blk, allow_ena_assert_cnt : std_logic;
Signal sop_source_int    : std_logic;
Signal eop_source_int    : std_logic;
Signal enable_source  : std_logic; 
Signal data_available : std_logic;
Signal sink_ena_master_int : std_logic;
Signal sink_ready_q : std_logic;
signal sink_val_int : std_logic;
Signal decbit_q : std_logic;
signal decbit_shunt : std_logic;
signal sink_val_q : std_logic;
Signal sink_ready_internal_del : std_logic;
Signal enable_trb : std_logic;


begin


ASSERT ((n_max>1) and (n_max<8))
-- coverage off 
    REPORT         "Parameter n cannot be bigger than 7" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (L_max<10)
-- coverage off
    REPORT         "Parameter L_max cannot be bigger than 9" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (L_max>2)
-- coverage off
    REPORT         "Parameter L_max cannot be lower than 3" 
    SEVERITY        ERROR;
-- coverage on
ASSERT (v>7) 
-- coverage off
    REPORT         "Parameter v must be at least 8" 
    SEVERITY        ERROR;
-- coverage on
--ASSERT (traceback_type="FlipFlops") or (traceback_type="Memory")
--    REPORT  "Parameter traceback_type has to be either FlipFlops or Memory"
--    SEVERITY ERROR;


--ifg0 : if punctured_rate/="unpunctured" generate
--
--puncturing : auk_vit_par_pun_atl
--    Generic map (
--            n => n_list(1), softbits => softbits,
--            punctured_rate => punctured_rate,
--            puncturing_pattern => puncturing_pattern)
--    Port map (
--            clk => clk, reset => reset, enable_in => enable,
--             rr => rr,    enable_out => pun_enabling, eras_sym => eras_sym_int,
--             yy => rr_2_bmp );    
--
--    valid <= pun_enabling;
--
--end generate ifg0;    
--
--ifga : if punctured_rate="unpunctured" generate
    --rr_2_bmp <= rr;
--    valid <= '1';
--    pun_enabling <= '1';
    --eras_sym_int <= eras_sym;
--end generate ifga;

--enable_bmp <= enable and pun_enabling;


-- Alex 03-03-07 it should be sink_rdy  internal  signal for every optimization case
sink_val_int <= sink_val and sink_ready_internal_del; --sink_ready_q;

metric_processing: auk_vit_par_bmp_atl
    generic map (n => n, L => L, L_max => L_max, softbits => softbits,
                             log2_n_max => log2_n_max, modes => dec_mode,
                             bmgwide => bmgwide, best_state_finder => best_state_finder, 
                             opt_par => parallel_optimization, BER => BER, node_sync => node_sync, 
                             ncodes => ncodes, n_max => n_max, 
                             ga => ga, gb => gb, gc => gc, gd => gd, ge => ge, gf => gf, gg => gg)        
    port map (clk => clk, 
                        reset => reset,
                        enable => enable_source,  --  to be checked

                        ena_master_sink => sink_ena_master_int, 
                        val_sink => sink_val_int,    
                        sop_sink => sink_sop_q,
                        eop_sink => sink_eop,
                        allow_ena_assert_from_trb => allow_ena_assert,

                        state_node_sync => state_node_sync_q,
                        tr_init_state => tr_init_state_q,
                         rr => rr, sel_code => sel_code_q, 
                        eras_sym => eras_sym,
                        rrff => rrff_q,
                        bestadd => bestadd_int, 
                        bestmet => bestmet,
                        survready => survready, baddready => baddready,
                        normalizations => normalizations,
                        sink_eop_del => sink_eop_del,
                         survive => survive, data_for_BER => data_for_BER);

--traceback_enable <= enable and pun_enabling;

--ifg2: if traceback_type="FlipFlops" generate
--
--tracing_back_ff: entity viterbi.auk_vit_par_trb_atl(rtl_ffs)
--    Generic map (n => n_list(1), L => L, softbits => softbits, bmgwide => bmgwide,
--                 v => v, config => config, dev_family => family)
--    Port map (clk => clk, reset => reset, enable => traceback_enable,
--                        --sync_rot => sync_rot_in, prenormalize => prenormalize, 
--                        survive => survive, max_info => max_info,
--                        decbit => decbitnode_ff, outvalid => outvalidnode_ff);
--
--outvalidnode <= outvalidnode_ff;
--decbitnode <= decbitnode_ff;

--ber_used_ff: if BER="used" generate
--
--ber_measurement_ff: entity viterbi.auk_vit_par_ber_atl(rtl_ffs)
--    Generic map (n => n_list(1), L => L, v => v, node_sync => node_sync,
--                ga => ga,    gb => gb,    gc => gc,    gd => gd,    ge => ge,    gf => gf,    gg => gg, 
--                dev_family => family)
--    Port map (
--        clk => clk, reset => reset, enable => enable_bmp,
--        outvalid => outvalidnode_ff, sync_rot => sync_rot_in,
--        data_ram_in => data_for_ber, decbit => decbitnode_ff,
--        period_ber => period_ber,
--        threshold_ns => threshold_ns, period_ns => period_ns,
--        in_sync => in_sync, out_sync => out_sync,
--        numerr_ns => numerr_ns,
--         bererr => bererr, numerr_ber => numerr_ber);
--
--end generate ber_used_ff;

--end generate ifg2;


fg3: FOR k IN 1 TO n_max GENERATE
  fg3b: for J in 1 to softbits generate
      rrff_d(k, J) <= rr((k-1)*softbits+J);
    end generate fg3b;
    rrff_d(k, softbits+1) <= eras_sym(k);
END GENERATE fg3;

clk_atl_rrff: Process (clk, reset)
    begin
    if reset='1' then
        for J in 1 to n_max loop
            for K in 1 to softbits+1 loop
                rrff_q(J, K) <= '0';
            end loop;
        end loop;
    elsif Rising_edge(clk) then
     -- This statement down here should be only for continuous and block but not for 
     -- for none optimization
        if sink_val='1' and sink_ready_internal_del='1' then
            rrff_q <= rrff_d;
        end if;
    end if;

end process clk_atl_rrff;


ifg_not_cont2: if parallel_optimization/="Continuous" generate


clk_atl: Process (clk, reset)
    begin
    if reset='1' then
        -- (master) sink side
        sink_sop_q <= '0';
        tb_length_q <= (others => '0');
        state_node_sync_q <= (others => '0');
        sel_code_q <= (others => '0');
        tr_init_state_q <= (others => '0');
        tb_type_q <= '0';
        sink_ready_q <= '0';
    elsif Rising_edge(clk) then
        sink_ready_q <= enable_source;
        if sink_val='1' then
          sink_sop_q <= sink_sop;
        end if;
        if sink_sop='1' and sink_val='1' then
            tb_length_q <= tb_length;
            state_node_sync_q <= state_node_sync;
            sel_code_q <= sel_code;
        end if;
        -- tr_init_state latched at the end of the block
        -- remember to document
        if sink_eop='1' and sink_val='1' then
            tr_init_state_q <= tr_init_state;
            tb_type_q <= tb_type;
        end if;
    end if;

end process clk_atl;

end generate ifg_not_cont2;

ifg_not_full: if parallel_optimization/="None" generate
    sink_ready_internal_del <= sink_ready_q;
end generate ifg_not_full;

    
ifg_full: if parallel_optimization="None" generate

delay_internal_sink_ready: Process (clk, reset)
    begin
    if reset='1' then
        sink_ready_internal_del <= '0';
    elsif Rising_edge(clk) then
        sink_ready_internal_del <= sink_ena_master_int;
    end if;
end process delay_internal_sink_ready;

sink_rdy <= sink_ena_master_int;

tracing_back_mem: entity work.auk_vit_par_trb_atl(rtl_mem)
    Generic map (L => L_max, V => V, ncodes => ncodes, vlog_wide => vlog_wide, use_altera_syncram => use_altera_syncram)
    Port map (clk => clk, reset => reset, 
                        enable => enable_source,
                        survready => survready,
              baddready => baddready,
                        --ena_slave_source => source_rdy,
                        allow_ena_assert => allow_ena_assert_mem,
                        data_available   => data_available,
                        --val_source => source_val_node,
                        sop_source => sop_source_int,
                        eop_source => eop_source_int,
                        survive => survive, 
                        tb_length => tb_length_q, tb_type => tb_type_q,
                        tr_init_state => tr_init_state_q, 
                        sink_eop_del => sink_eop_del,
                        sink_sop_q => sink_sop_q,
                         decbit => decbitnode,
                         bestadd => bestadd_int);

--outvalidnode <= outvalidnode_mem;
--decbitnode <= decbitnode_mem;
--source_val_node <= source_val_mem;
allow_ena_assert <= allow_ena_assert_mem;

end generate ifg_full;


ifg_cont: if parallel_optimization="Continuous" generate

-- rrff_q move here, but this should be general for all parallel variants!! 

sink_sop_q <= '0';

clk_atl: Process (clk, reset)
    begin
    if reset='1' then
        -- (master) sink side
        state_node_sync_q <= (others => '0');
        sel_code_q <= (others => '0');
        sink_val_q <= '0';
        -- for J in 1 to n_max loop
            -- for K in 1 to softbits+1 loop
                -- rrff_q(J, K) <= '0';
                -- --(n_max downto 1 => '0', softbits+1 downto 1 => '0'); --, others => '0');
            -- end loop;
        -- end loop;
        --rrff_q <= (others => '0');
        sink_ready_q <= '0';
    elsif Rising_edge(clk) then
      sink_ready_q <= enable_source;
        sink_val_q <= sink_val and sink_ready_q;
        --if sink_val='1' and sink_ready_q='1' then
        --    rrff_q <= rrff_d;
        --end if;
        --if sink_val='1' and sink_ready_q='1' then
        --  sink_sop_q <= sink_sop;
        --end if;
        --if sink_sop='1' and sink_val='1' then
        if sink_val='1' then
            --tb_length_q <= tb_length;
            state_node_sync_q <= state_node_sync;
            sel_code_q <= sel_code;
        end if;
        -- tr_init_state latched at the end of the block
        -- remember to document
        -- if sink_eop='1' and sink_val='1' then
            -- tr_init_state_q <= tr_init_state;
            -- tb_type_q <= tb_type;
        -- end if;
    end if;

end process clk_atl;

tracing_back_cont: entity work.auk_vit_par_trb_atl(rtl_cnt)
    Generic map (L => L_max, V => V, ncodes => ncodes, vlog_wide => vlog_wide, ini_filename=>ini_filename, use_altera_syncram=>use_altera_syncram)
    Port map (clk => clk, 
                        reset => reset,
                        enable => enable_trb,
                        survready => survready,
                        baddready => baddready,
                        --ena_slave_source => source_rdy,
                        allow_ena_assert => open, --allow_ena_assert_cnt,
                        data_available   => data_available,
                        --val_source => open,
                        sop_source => open,
                        eop_source => open,
                        survive => survive, 
                        tb_length => tb_length_q, 
                        tb_type => tb_type_q,
                        tr_init_state => tr_init_state_q, 
                        sink_eop_del => sink_eop_del,
                        sink_sop_q => sink_sop_q,
                         decbit => decbitnode,
                         bestadd => bestadd_int);

-- Alex 01-02-2007  I am moving the sink_val_q disabling effect on data_available inside the trb_cont
--data_available <= data_available_trb; -- and sink_val_q;
-- Alex 01-02-2007  I am moving the sink_val_q disabling effect inside the trb_cont
enable_trb <= enable_source; -- and sink_val_q;
--outvalidnode <= outvalidnode_mem;
--decbitnode <= decbit_q;
--source_val_node <= source_val_cnt;
-- Alex 23-01-2007 do I really need this signal here?
allow_ena_assert <= '1'; --allow_ena_assert_cnt;

sop_source_int <= '0';
eop_source_int <= '0';
tr_init_state_q <= (others => '0');
tb_type_q <= '0';
tb_length_q <= (others => '0');

sink_rdy <= enable_source;


end generate ifg_cont;


ifg_block: if parallel_optimization="Block" generate

tracing_back_block: entity work.auk_vit_par_trb_atl(rtl_blk)
    Generic map (L => L_max, V => V, ncodes => ncodes, vlog_wide => vlog_wide, use_altera_syncram=>use_altera_syncram)
    Port map (clk => clk, 
                        reset => reset,
                        enable => enable_source,
                        survready => survready,
              baddready => baddready,
                        --ena_slave_source => source_rdy,
                        allow_ena_assert => allow_ena_assert_blk, 
                        data_available   => data_available,
                        --val_source => open, 
                        sop_source => sop_source_int,
                        eop_source => eop_source_int,
                        survive => survive, 
                        tb_length => tb_length_q, 
                        tb_type => tb_type_q,
                        tr_init_state => tr_init_state_q, 
                        sink_eop_del => sink_eop_del,
                        sink_sop_q => sink_sop_q,
                         decbit => decbitnode,
                         bestadd => bestadd_int);

--decbitnode <= decbitnode_blk;
-- seems obsoleted now
allow_ena_assert <= allow_ena_assert_blk;
sink_rdy <= enable_source;
                        
end generate ifg_block;


Avalon_streaming_control: auk_dspip_avalon_str_ctrl
    Generic map (
                ready_latency => ready_latency)
        Port map (
            clk  => clk,
            reset    => reset,
            ----------------- DESIGN SIDE SOURCE SIGNALS
            data_available => data_available,
            sop_source_int => sop_source_int,
            eop_source_int => eop_source_int,
            enable_source  => enable_source,
            ----------------- AVALON_STREAMING SOURCE SIDE SIGNALS 
            source_ready    => source_rdy,
            source_val        => source_val_node,
            source_sop        => source_sop_node,
            source_eop        => source_eop_node
    );


-- decbit output pipe with shunt buffer
decbit_last_buffer : Process(clk, reset)

    variable decbit_d : Std_Logic;
  
begin
if reset = '1' then
    decbit_q <= '0';
    decbit_shunt <= '0';
elsif Rising_edge(clk) then
  if enable_source='1' then
        decbit_d := decbitnode; 
    else
        decbit_d := decbit_shunt;
    end if;
  if enable_source='1' then
        decbit_shunt <= decbitnode;
  end if;
    if source_rdy='1' then
        decbit_q <= decbit_d;
    end if;
end if;
end process decbit_last_buffer;

ber_used: if BER="used" generate

ber_measurement: auk_vit_par_ber_atl

    Generic map (n => n, L => L, 
          dec_modes => dec_mode,
                opt_par => parallel_optimization,
          ncodes => ncodes, 
                n_max => n_max, L_max => L_max,
          vlog_wide => vlog_wide, 
                size_bus => size_bus, sel_code_size => sel_code_size,
                numerr_size => numerr_size,
                ga => ga,    gb => gb,    gc => gc,    gd => gd,    ge => ge,    gf => gf,    gg => gg,
                dev_family => family)
    Port map (
        clk => clk, 
        reset => reset,
        ber_clear => ber_clear,
        sop_source => source_sop_node,
        eop_source => source_eop_node,
        sink_val => sink_val,
        sink_sop => sink_sop,
        sink_eop => sink_eop,
        sel_code => sel_code, 
        outvalid => source_val_node, 
        data_ram_in => data_for_ber, 
        decbit => decbit_q,
         numerr => numerr);

end generate ber_used;


ber_not_used: if not(BER="used") generate
    numerr <= (others=>'0');
end generate ber_not_used;


--no_nodesync: if node_sync="unused" generate
--    sync_rot_in <= '0';
--end generate no_nodesync;
--
--node_sync_logic: if node_sync="used" generate
--    sync_rot_in <= sync_rot;
--end generate node_sync_logic;

bestadd <= bestadd_int;
decbit <= decbit_q;
source_sop <= source_sop_node;
source_eop <= source_eop_node;
source_val <= source_val_node;

end architecture rtl;    

