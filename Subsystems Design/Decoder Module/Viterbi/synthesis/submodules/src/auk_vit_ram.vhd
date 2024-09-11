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
-- Description	:  Convolutional Encoder function for Viterbi Decoder
--                 with multicode support
--
-- Legal Notice: (C)2006 Altera Corporation. All rights reserved. Your
-- use of Altera Corporation's design tools, logic functions and other
-- software and tools, and its AMPP partner logic functions, and any
-- output files any of the foregoing (including device programming or
-- simulation files), and any associated documentation or information are
-- expressly subject to the terms and conditions of the Altera Program
-- License Subscription Agreement or other applicable license agreement,
-- including, without limitation, that your use is for the sole purpose
-- of programming logic devices manufactured by Altera and sold by Altera
-- or its authorized distributors. Please refer to the applicable
-- agreement for further details. 
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
library altera_lnsim;
use altera_lnsim.altera_lnsim_components.all;

Entity auk_vit_ram is
	Generic (
		WIDTH_DATA : NATURAL := 2;
		WIDTH_ADDR : NATURAL := 7;
		NUMWORDS   : NATURAL  := 7;
		INIT_FILE : STRING := "filename.hex"; 
		USE_INIFILE : NATURAL := 0;
		USE_CLOCKEN1 : NATURAL := 1;
		USE_ALTERA_SYNCRAM : NATURAL := 1
	);
	Port (
		clock0 : in Std_Logic := '1';
		clock1 : in Std_Logic := 'Z';
		clocken1 : in Std_Logic := '1';
		wren_a : in Std_Logic := '0';
		data_a : in Std_Logic_Vector(WIDTH_DATA downto 1);
		address_a : in Std_Logic_Vector(WIDTH_ADDR downto 1);
		address_b : in Std_Logic_Vector(WIDTH_ADDR downto 1);
		q_b : out Std_Logic_Vector(WIDTH_DATA downto 1)
	);	
end entity auk_vit_ram;	

Architecture rtl of auk_vit_ram is

    begin
    
    -- if_a10: if USE_ALTERA_SYNCRAM=1 generate
        -- if_init_file : if USE_INIFILE=1 generate
            -- if_clockena : if USE_CLOCKEN1=1 generate
                -- RAM : altera_syncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", init_file => INIT_FILE, address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,
                  -- clocken1 => clocken1,  
                  -- q_b => q_b );
            -- end generate;
            -- if_not_clockena : if USE_CLOCKEN1=0 generate
                -- RAM : altera_syncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED",  width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", init_file => INIT_FILE,  address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,  
                  -- q_b => q_b );
            -- end generate;
        -- end generate;
        
        -- if_not_init_file : if USE_INIFILE=0 generate
            -- if_clockena : if USE_CLOCKEN1=1 generate
                -- RAM : altera_syncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED", 
                    -- indata_reg_b => "UNUSED",   address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,
                  -- clocken1 => clocken1,  
                  -- q_b => q_b );
            -- end generate;
            -- if_not_clockena : if USE_CLOCKEN1=0 generate
                -- RAM : altera_syncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED",  address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,  
                  -- q_b => q_b );
            -- end generate;
        -- end generate;

    -- end generate;
    
    

    -- if_not_a10: if USE_ALTERA_SYNCRAM=0 generate

        -- if_init_file : if USE_INIFILE=1 generate
            -- if_clockena : if USE_CLOCKEN1=1 generate
                -- RAM : altsyncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  -- wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", init_file => INIT_FILE,
                    -- indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,
                  -- clocken1 => clocken1,  
                  -- q_b => q_b );
            -- end generate;
            -- if_not_clockena : if USE_CLOCKEN1=0 generate
                -- RAM : altsyncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  -- wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", init_file => INIT_FILE,
                    -- indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,  
                  -- q_b => q_b );
            -- end generate;
        -- end generate;
        
        -- if_not_init_file : if USE_INIFILE=0 generate
            -- if_clockena : if USE_CLOCKEN1=1 generate
                -- RAM : altsyncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  -- wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO",  lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,
                  -- clocken1 => clocken1,  
                  -- q_b => q_b );
            -- end generate;
            -- if_not_clockena : if USE_CLOCKEN1=0 generate
                -- RAM : altsyncram 
                -- GENERIC map (
                  -- operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  -- outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  -- outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  -- wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  -- address_reg_b => "CLOCK1", wrcontrol_wraddress_reg_b => "UNUSED",
                    -- width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    -- rdcontrol_reg_b => "UNUSED",
                    -- outdata_reg_b => "CLOCK1", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    -- indata_reg_b => "UNUSED", indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  -- read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                -- PORT map (
                  -- wren_a => wren_a, data_a => data_a, 
                  -- address_a => address_a, address_b => address_b, 
                  -- clock0 => clock0, clock1 => clock1 ,  
                  -- q_b => q_b );
            -- end generate;
        -- end generate;
    -- end generate;
if_a10: if USE_ALTERA_SYNCRAM=1 generate
        if_init_file : if USE_INIFILE=1 generate
            if_clockena : if USE_CLOCKEN1=1 generate
                RAM : altera_syncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", init_file => INIT_FILE, address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0, 
                  clocken0 => clocken1,  
                  q_b => q_b );
            end generate;
            if_not_clockena : if USE_CLOCKEN1=0 generate
                RAM : altera_syncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED",  width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", init_file => INIT_FILE,  address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0,   
                  q_b => q_b );
            end generate;
        end generate;
        
        if_not_init_file : if USE_INIFILE=0 generate
            if_clockena : if USE_CLOCKEN1=1 generate
                RAM : altera_syncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", 
                    indata_reg_b => "UNUSED",   address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0, 
                  clocken0 => clocken1,  
                  q_b => q_b );
            end generate;
            if_not_clockena : if USE_CLOCKEN1=0 generate
                RAM : altera_syncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED",  address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0,   
                  q_b => q_b );
            end generate;
        end generate;

    end generate;
    
    

    if_not_a10: if USE_ALTERA_SYNCRAM=0 generate

        if_init_file : if USE_INIFILE=1 generate
            if_clockena : if USE_CLOCKEN1=1 generate
                RAM : altsyncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", init_file => INIT_FILE,
                    indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0, 
                  clocken0 => clocken1,  
                  q_b => q_b );
            end generate;
            if_not_clockena : if USE_CLOCKEN1=0 generate
                RAM : altsyncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", init_file => INIT_FILE,
                    indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0,   
                  q_b => q_b );
            end generate;
        end generate;
        
        if_not_init_file : if USE_INIFILE=0 generate
            if_clockena : if USE_CLOCKEN1=1 generate
                RAM : altsyncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED", wrcontrol_wraddress_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO",  lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0, 
                  clocken0 => clocken1,  
                  q_b => q_b );
            end generate;
            if_not_clockena : if USE_CLOCKEN1=0 generate
                RAM : altsyncram 
                GENERIC map (
                  operation_mode => "DUAL_PORT", width_a => WIDTH_DATA, widthad_a => WIDTH_ADDR, numwords_a => NUMWORDS,
                  outdata_reg_a => "UNUSED", address_aclr_a => "UNUSED",
                  outdata_aclr_a => "UNUSED", indata_aclr_a => "UNUSED",    
                  wrcontrol_aclr_a => "UNUSED", width_byteena_a => 1, 
                  address_reg_b => "CLOCK0", wrcontrol_wraddress_reg_b => "UNUSED",
                    width_b => WIDTH_DATA, widthad_b => WIDTH_ADDR, numwords_b => NUMWORDS,
                    rdcontrol_reg_b => "UNUSED",
                    outdata_reg_b => "CLOCK0", outdata_aclr_b => "UNUSED", rdcontrol_aclr_b => "UNUSED",
                    indata_reg_b => "UNUSED", indata_aclr_b => "UNUSED", wrcontrol_aclr_b => "UNUSED", address_aclr_b => "UNUSED",
                  read_during_write_mode_mixed_ports => "OLD_DATA", ram_block_type => "AUTO", lpm_hint => "UNUSED")
                PORT map (
                  wren_a => wren_a, data_a => data_a, 
                  address_a => address_a, address_b => address_b, 
                  clock0 => clock0,   
                  q_b => q_b );
            end generate;
        end generate;
    end generate;
end architecture rtl;	
