	component viterbi is
		port (
			clk            : in  std_logic                    := 'X';             -- clk
			reset          : in  std_logic                    := 'X';             -- reset
			sink_val       : in  std_logic                    := 'X';             -- valid
			sink_rdy       : out std_logic;                                       -- ready
			ber_clear      : in  std_logic                    := 'X';             -- ber_clear
			eras_sym       : in  std_logic_vector(3 downto 0) := (others => 'X'); -- eras_sym
			rr             : in  std_logic_vector(3 downto 0) := (others => 'X'); -- rr
			source_val     : out std_logic;                                       -- valid
			source_rdy     : in  std_logic                    := 'X';             -- ready
			numerr         : out std_logic_vector(7 downto 0);                    -- numerr
			normalizations : out std_logic_vector(7 downto 0);                    -- normalizations
			decbit         : out std_logic                                        -- decbit
		);
	end component viterbi;

	u0 : component viterbi
		port map (
			clk            => CONNECTED_TO_clk,            -- clk.clk
			reset          => CONNECTED_TO_reset,          -- rst.reset
			sink_val       => CONNECTED_TO_sink_val,       --  in.valid
			sink_rdy       => CONNECTED_TO_sink_rdy,       --    .ready
			ber_clear      => CONNECTED_TO_ber_clear,      --    .ber_clear
			eras_sym       => CONNECTED_TO_eras_sym,       --    .eras_sym
			rr             => CONNECTED_TO_rr,             --    .rr
			source_val     => CONNECTED_TO_source_val,     -- out.valid
			source_rdy     => CONNECTED_TO_source_rdy,     --    .ready
			numerr         => CONNECTED_TO_numerr,         --    .numerr
			normalizations => CONNECTED_TO_normalizations, --    .normalizations
			decbit         => CONNECTED_TO_decbit          --    .decbit
		);

