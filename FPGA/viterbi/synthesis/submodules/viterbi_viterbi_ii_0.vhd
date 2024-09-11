library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity viterbi_viterbi_ii_0 is
    generic (
        viterbi_type          : STRING := "Parallel";
        parallel_optimization : STRING := "Continuous";
        n                     : STRING := "4";
        ncodes                : INTEGER := 1;
        L                     : STRING := "3";
        dec_mode              : STRING := "V";
        ga                    : STRING := "7";
        gb                    : STRING := "6";
        gc                    : STRING := "5";
        gd                    : STRING := "4";
        ge                    : STRING := "0";
        gf                    : STRING := "0";
        gg                    : STRING := "0";
        acs_units             : INTEGER := 1;
        v                     : INTEGER := 32;
        softbits              : INTEGER := 1;
        rr_size               : INTEGER := 4;
        n_max                 : INTEGER := 4;
        log2_n_max            : INTEGER := 2;
        bmgwide               : INTEGER := 7;
        numerr_size           : INTEGER := 8;
        constraint_length_m_1 : INTEGER := 2;
        vlog_wide             : INTEGER := 6;
        sel_code_size         : INTEGER := 1;
        ber                   : STRING := "used";
        node_sync             : STRING := "unused";
        best_state_finder     : STRING := "unused";
        use_altera_syncram    : INTEGER := 0
    );
    port (
        state_node_sync : in  STD_LOGIC_VECTOR(1 downto 0);
        tb_length       : in  STD_LOGIC_VECTOR(5 downto 0);
        tr_init_state   : in  STD_LOGIC_VECTOR(1 downto 0);
        bestadd         : out STD_LOGIC_VECTOR(1 downto 0);
        bestmet         : out STD_LOGIC_VECTOR(6 downto 0);
        bm_init_state   : in  STD_LOGIC_VECTOR(1 downto 0);
        bm_init_value   : in  STD_LOGIC_VECTOR(6 downto 0);
        eras_sym        : in  STD_LOGIC_VECTOR(3 downto 0);
        rr              : in  STD_LOGIC_VECTOR(3 downto 0);
        numerr          : out STD_LOGIC_VECTOR(7 downto 0);
        normalizations  : out STD_LOGIC_VECTOR(7 downto 0);
        sink_eop        : in  STD_LOGIC;
        reset           : in  STD_LOGIC;
        sink_rdy        : out STD_LOGIC;
        tb_type         : in  STD_LOGIC;
        ber_clear       : in  STD_LOGIC;
        clk             : in  STD_LOGIC;
        source_sop      : out STD_LOGIC;
        decbit          : out STD_LOGIC;
        source_val      : out STD_LOGIC;
        source_eop      : out STD_LOGIC;
        sel_code        : in  STD_LOGIC;
        sink_sop        : in  STD_LOGIC;
        source_rdy      : in  STD_LOGIC;
        sink_val        : in  STD_LOGIC
    );
end entity viterbi_viterbi_ii_0;

architecture rtl of viterbi_viterbi_ii_0 is

    component  auk_vit_top 
        generic (
            viterbi_type          : STRING := "Parallel";
            parallel_optimization : STRING := "Continuous";
            n                     : STRING := "4";
            ncodes                : INTEGER := 1;
            L                     : STRING := "3";
            dec_mode              : STRING := "V";
            ga                    : STRING := "7";
            gb                    : STRING := "6";
            gc                    : STRING := "5";
            gd                    : STRING := "4";
            ge                    : STRING := "0";
            gf                    : STRING := "0";
            gg                    : STRING := "0";
            acs_units             : INTEGER := 1;
            v                     : INTEGER := 32;
            softbits              : INTEGER := 1;
            rr_size               : INTEGER := 4;
            n_max                 : INTEGER := 4;
            log2_n_max            : INTEGER := 2;
            bmgwide               : INTEGER := 7;
            numerr_size           : INTEGER := 8;
            constraint_length_m_1 : INTEGER := 2;
            vlog_wide             : INTEGER := 6;
            sel_code_size         : INTEGER := 1;
            ber                   : STRING := "used";
            node_sync             : STRING := "unused";
            best_state_finder     : STRING := "unused";
            use_altera_syncram    : INTEGER := 0;
            ini_filename          : STRING := "viterbi_viterbi_ii_0_ini.hex"
        );
        port (
            state_node_sync : in  STD_LOGIC_VECTOR(1 downto 0);
            tb_length       : in  STD_LOGIC_VECTOR(5 downto 0);
            tr_init_state   : in  STD_LOGIC_VECTOR(1 downto 0);
            bestadd         : out STD_LOGIC_VECTOR(1 downto 0);
            bestmet         : out STD_LOGIC_VECTOR(6 downto 0);
            bm_init_state   : in  STD_LOGIC_VECTOR(1 downto 0);
            bm_init_value   : in  STD_LOGIC_VECTOR(6 downto 0);
            eras_sym        : in  STD_LOGIC_VECTOR(3 downto 0);
            rr              : in  STD_LOGIC_VECTOR(3 downto 0);
            numerr          : out STD_LOGIC_VECTOR(7 downto 0);
            normalizations  : out STD_LOGIC_VECTOR(7 downto 0);
            sink_eop        : in  STD_LOGIC;
            reset           : in  STD_LOGIC;
            sink_rdy        : out STD_LOGIC;
            tb_type         : in  STD_LOGIC;
            ber_clear       : in  STD_LOGIC;
            clk             : in  STD_LOGIC;
            source_sop      : out STD_LOGIC;
            decbit          : out STD_LOGIC;
            source_val      : out STD_LOGIC;
            source_eop      : out STD_LOGIC;
            sel_code        : in  STD_LOGIC_VECTOR(0 downto 0);
            sink_sop        : in  STD_LOGIC;
            source_rdy      : in  STD_LOGIC;
            sink_val        : in  STD_LOGIC
        );
    end component auk_vit_top;

    signal sel_code_vect : STD_LOGIC_VECTOR(0 downto 0);

begin

    sel_code_vect(0) <= sel_code;

    auk_vit_top_inst : component auk_vit_top
        generic map (
            viterbi_type          => viterbi_type,
            parallel_optimization => parallel_optimization,
            n                     => n,
            ncodes                => ncodes,
            L                     => L,
            dec_mode              => dec_mode,
            ga                    => ga,
            gb                    => gb,
            gc                    => gc,
            gd                    => gd,
            ge                    => ge,
            gf                    => gf,
            gg                    => gg,
            acs_units             => acs_units,
            v                     => v,
            softbits              => softbits,
            rr_size               => rr_size,
            n_max                 => n_max,
            log2_n_max            => log2_n_max,
            bmgwide               => bmgwide,
            numerr_size           => numerr_size,
            constraint_length_m_1 => constraint_length_m_1,
            vlog_wide             => vlog_wide,
            sel_code_size         => sel_code_size,
            ber                   => ber,
            node_sync             => node_sync,
            best_state_finder     => best_state_finder,
            use_altera_syncram    => use_altera_syncram,
            ini_filename          => "viterbi_viterbi_ii_0_ini.hex"
        )
        port map (
            state_node_sync => state_node_sync,
            tb_length       => tb_length,
            tr_init_state   => tr_init_state,
            bestadd         => bestadd,
            bestmet         => bestmet,
            bm_init_state   => bm_init_state,
            bm_init_value   => bm_init_value,
            eras_sym        => eras_sym,
            rr              => rr,
            numerr          => numerr,
            normalizations  => normalizations,
            sink_eop        => sink_eop,
            reset           => reset,
            sink_rdy        => sink_rdy,
            tb_type         => tb_type,
            ber_clear       => ber_clear,
            clk             => clk,
            source_sop      => source_sop,
            decbit          => decbit,
            source_val      => source_val,
            source_eop      => source_eop,
            sel_code        => sel_code_vect,
            sink_sop        => sink_sop,
            source_rdy      => source_rdy,
            sink_val        => sink_val
        );

end architecture rtl; -- of viterbi_viterbi_ii_0
