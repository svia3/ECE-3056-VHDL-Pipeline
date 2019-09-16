-- ECE 3056: Architecture, Concurrency and Energy in Computation
-- Sudhakar Yalamanchili
-- Pipelined MIPS Processor VHDL Behavioral Model from H&P Figure 4.46
--
--
-- A few things to note. In this pipelined model, the MemToReg mux in the last stage has
-- been moved to the decode module and the signals have been run directly into the
-- decode module.
-- Control signals in each stage are referenced by their positions in the
-- pipeline registers.
--
-- the base register is always used as $0 since the memories are so small.
--
--Tried to stay faithful to Figure 4.46 in the text. The model declares local
--signals in each stage. In practice you would show connections directly to the
--pipeline register bits. However this makes it difficult to read the traces
--and debug. Local signals in each stage are prefaced with the stage name,
--i.e., id_, wb_, etc.
--
--
------------------------------------------------------------------------------------
--Stephen Via
--ECE 3056 -- Pipeline Assignment 2
------------------------------------------------------------------------------------
Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_signed.all;


entity PipeMIPSV is
-- model is self contained hence no entity ports
end PipeMIPSV;


architecture behavioral of PipeMIPSV is
-- declarations for signals in each stage of the pipeline and the clock and
-- reset module

-- clock and reset module
  signal clk, reset : std_logic;

-- declare signals in the fetch stage
  -- instruction memory is only 8 words. code starts at the second location,
  -- This is just a hack to avoid more complex reset logic.
-------------------------------------------------------------------
--base test
-- X"8c070004",   --  lw  $7, 4($0)
-- X"8C080008",   --  lw  $8, 8($0)
-- X"01074820",   --  add $9, $8, $7
-- X"ac09000c",   --  sw  $9, 12($0)
-- X"1000FFFB",   --  beq $0, $0, -5 (branch back 5 words/20 bytes)
-- X"00000000",   --  nop
-- X"00000000",   --  nop
-- X"00000000"    --	 nop
-------------------------------------------------------------------
  TYPE INST_MEM IS ARRAY (0 to 7) of STD_LOGIC_VECTOR (31 DOWNTO 0);
   SIGNAL iram : INST_MEM := (
      X"00008020", -- add $s0 $zero $zero
      X"02008820", -- add $s1 $s0 $zero
      X"1000FFFD", -- beq $zero $zero -3
      X"01090020", -- add $zero $t0 $t1
      X"012A8020", -- add $s0 $t1 $t2
      X"8C10000C", -- lw $s0 12($zero)
      X"00000000",   --  nop
      X"00000000"   --  nop
   );

  SIGNAL if_PC, if_Next_PC, if_Instruction : STD_LOGIC_VECTOR( 31 DOWNTO 0 );
  signal IF_ID : std_logic_vector(63 downto 0);

  -- declare signals in the decode stage

  TYPE register_file IS ARRAY ( 0 TO 31 ) OF STD_LOGIC_VECTOR( 31 DOWNTO 0 );

	SIGNAL register_array: register_file := (
      X"00000000", X"11111111", X"22222222", X"33333333",
      X"44444444", X"55555555", X"66666666", X"77777777",
      X"0000000A", X"1111111A", X"2222222A", X"3333333A",
      X"4444444A", X"5555555A", X"6666666A", X"7777777A",
      X"0000000B", X"1111111B", X"2222222B", X"3333333B",
      X"4444444B", X"5555555B", X"6666666B", X"7777777B",
      X"000000BA", X"111111BA", X"222222BA", X"333333BA",
      X"444444BA", X"555555BA", X"666666BA", X"777777BA"
   );
  SIGNAL id_write_data, id_PC, id_Sign_extend, id_register_rs, id_register_rt	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
  signal id_wreg_rt, id_wreg_rd     : std_logic_vector(4 downto 0);
  signal id_instruction             :std_logic_vector(31 downto 0);
  --new 151 instead of 146 bits
  signal ID_EX                      : std_logic_vector(151 downto 0); -- Look at Figure 4.46

  -- declare local signals in the Execute Stage

  SIGNAL Ainput, Binput, ex_ALU_result	: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
  --new--
  SIGNAL ForwardA, ForwardB : STD_LOGIC_VECTOR(1 DOWNTO 0);
  signal id_reg_num_rs, ex_reg_num_rs          : STD_LOGIC_VECTOR(4 downto 0);
  signal luHazard                              : std_logic;
  --new--
  signal ex_Branch_address  		: std_logic_vector (31 downto 0);
  Signal Function_opcode 		: std_logic_vector (5 downto 0);
  signal ex_Zero 		        : std_logic;
  SIGNAL ALU_ctl	                : STD_LOGIC_VECTOR( 2 DOWNTO 0 );

  signal ex_register_rs, ex_register_rt, ex_PC4, ex_sign_extend  : std_logic_vector(31 downto 0);
  --new-- -> added ex_MemRead
  signal ex_RegDst, ex_ALUSrc, ex_MemRead   : std_logic;
  --new--
  signal ex_ALUOp               : std_logic_vector(1 downto 0);
  signal ex_wreg_rt, ex_wreg_rd, ex_wreg_addr  : std_logic_vector(4 downto 0);

  signal EX_MEM         : std_logic_vector(106 downto 0); -- Look at Figure 4.46

  -- declare signals in the Memory Stage

   TYPE DATA_RAM IS ARRAY (0 to 31) OF STD_LOGIC_VECTOR (31 DOWNTO 0);
  -- intialize to some random values
    SIGNAL dram: DATA_RAM := (
      X"00000000", X"11111111", X"22222222", X"33333333",
      X"44444444", X"55555555", X"66666666", X"77777777",
      X"0000000A", X"1111111A", X"2222222A", X"3333333A",
      X"4444444A", X"5555555A", X"6666666A", X"7777777A",
      X"0000000B", X"1111111B", X"2222222B", X"3333333B",
      X"4444444B", X"5555555B", X"6666666B", X"7777777B",
      X"000000BA", X"111111BA", X"222222BA", X"333333BA",
      X"444444BA", X"555555BA", X"666666BA", X"777777BA"
   );

  signal MEM_WB                 : std_logic_vector(70 downto 0); -- Look at Figure 4.46
  signal mem_read_data          : std_logic_vector(31 downto 0);

  signal mem_ALUOutput, mem_writedata, mem_branch_address              : std_logic_vector(31 downto 0);
  --new -> added mem_RegWrite, and branch_Taken
  signal mem_branch, mem_zero, mem_MemRead, mem_MemWrite, mem_PCSrc, mem_RegWrite, branch_Taken     : std_logic;
  --new
  signal mem_wreg_addr :std_logic_vector(4 downto 0);

  -- declare local signals in the WB stage
  signal wb_RegWrite, wb_MemToReg       :std_logic;
  signal wb_read_data, wb_ALUOutput     :std_logic_vector(31 downto 0);
  signal wb_wreg_addr                   :std_logic_vector(4 downto 0);

  -- declare signals in the controller

  signal rformat, lw, sw, beq, MemRead, MemWrite        :std_logic;
  signal RegWrite, MemToReg, RegDst, AluSrc, Branch     : std_logic;
  signal AluOp :std_logic_vector(1 downto 0);


  --------------------------------------------------------------
  -- clock and reset signal generation
  --------------------------------------------------------------
  begin

process
        begin
        -- generate clock
          clk <= '0', '1' after 50 ns;
          wait for 100 ns;  -- causes the preceding statement to execute indefinitely
         end process;
         -- following statement executes only once since nothing on the RHS every changes
reset <= '1', '0' after 75 ns;

 -----------------------------------------------------------------------------
 -- fetch stage
 -----------------------------------------------------------------------------

          -- access instruction pointed to by current PC
          -- and increment PC by 4. This is combinational
	  -- The concatenation of the leading 0 is to prevent the index from being
          -- interpreted as a 2's complement number

if_Instruction <=  iram(CONV_INTEGER('0'& if_PC(4 downto 2)));  -- since the instruction
                                                     -- memory is indexed by integer
---------------------------------------------------------------------------------------------------------------------
--------------------------------------- LOAD_TO_USE HAZARD DETECTION UNIT -------------------------------------------
---------------------------------------------------------------------------------------------------------------------
--new--
luHazard <= '1' when ((ex_wreg_rt = id_wreg_rt or ex_wreg_rt = id_reg_num_rs) and id_reg_num_rs /= X"000000000" and ex_MemRead = '1') else
              '0';
--new--
if_Next_PC <=  if_PC when luHazard = '1' else --new--
                (if_PC + 4)    when mem_PCSrc = '0' else
                  mem_branch_address    when mem_PCSrc = '1' else
                    X"CCCCCCCC";
---------------------------------------------------------------------------------------------------------------------
branch_Taken <= '1' when (mem_Zero = '1' and mem_branch = '1') else
                  '0';

-- update the PC on the next clock
	PROCESS
		BEGIN
			WAIT UNTIL (rising_edge(clk));
			IF (reset = '1' or branch_Taken = '1') THEN
                          if_PC<= X"00000000" ;
                          IF_ID <= X"0000000000000000";-- initialize pipeline register
			ELSE
                          if_PC <= if_Next_PC;
     --new--
              IF (luHazard = '1') THEN --if there is a load-to-use -> take decode instruction and "refetch" it
                          IF_ID(63 downto 32) <= id_PC;
                          IF_ID (31 downto 0) <= id_instruction;
              ELSE
                          IF_ID(63 downto 32) <= (if_PC+4);  -- update the contents of
                          IF_ID (31 downto 0) <= if_Instruction; -- the pipeline reg
              end if;
      --new--
			 end if;

                        END PROCESS;

 --------------------------------------------------------------------------
 -- decode stage
 --------------------------------------------------------------------------

 -- Declare local signals to make it easier to understand and debug (but more verbose). In
 -- practice you actually use the pipeline register bits directly but this
 -- makes the code harder to read and debug. Local signals are
 -- prefixed with the stage - here it is "id". Reference the documentation and
 -- Figure 4.46 for the meaning of bits on each pipeline register.
                        id_instruction <= IF_ID(31 downto 0);
                        wb_RegWrite <= MEM_WB(70);
                        wb_MemToReg <= MEM_WB(69);
                        wb_read_data <= MEM_WB(68 downto 37);
                        wb_ALUOutput <= MEM_WB(36 downto 5);
                        wb_wreg_addr <= MEM_WB(4 downto 0);

	-- MemToReg Mux for Writeback is implemented in decode. The correct ALU
        -- Result or memory read result is available from the MEM_WB register.
        -- See Figure 4.46
	   id_write_data <= wb_read_data
			           WHEN (wb_MemToReg = '1' ) 	-- MemToReg signal
			           ELSE  wb_ALUOutput;

	-- Sign Extend 16-bits to 32-bits. This is the lower 16 bits of IF_ID
    	id_Sign_extend <= X"0000" & id_instruction( 15 DOWNTO 0 )
		         WHEN id_instruction(15) = '0'
		         ELSE	X"FFFF" & id_instruction( 15 DOWNTO 0 );

	-- Read Register rs Operation
        -- The concatenation of the leading 0 is to prevent the index from being
        -- interpreted as a 2's complement number

	id_register_rs <= register_array(
			      CONV_INTEGER( '0'& id_instruction( 25 DOWNTO 21 ) ) );
	-- Read Register rt Operation
	id_register_rt <= register_array(
			      CONV_INTEGER( '0'& id_instruction( 20 DOWNTO 16 ) ) );
	-- Register write operation. Target register (wreg_address) is
        -- available from MEM_WB

	register_array( CONV_INTEGER('0'& wb_wreg_addr)) <= id_write_data
		                      when wb_RegWrite = '1' else -- RegWrite
                                                                  -- control signal
		                      register_array(conv_integer('0'& wb_wreg_addr));

	-- move possible write destinations to execute stage
	id_wreg_rd <= id_instruction(15 downto 11);
  id_wreg_rt <= id_instruction(20 downto 16);
  --new--
  id_reg_num_rs <= id_instruction(25 downto 21);
  --new--

        -- need to propagate PC+4 down the pipeline

        id_PC <= IF_ID(63 downto 32);

        PROCESS
		BEGIN
			WAIT UNTIL (rising_edge(clk));
			IF (reset = '1' or luHazard = '1' or branch_Taken = '1') THEN
                          ID_EX(146 downto 75)<= X"000000000000000000";-- initialize pipeline register
			                    ID_EX (74 downto 3)<= X"000000000000000000";
			                    ID_EX(2 downto 0) <= "000";
			ELSE
                          --new--
                          ID_EX(151 downto 147) <= id_reg_num_rs;
                          --new--
                          ID_EX(146) <= RegWrite; -- pipeline the control signals
                          ID_EX(145) <= MemToReg;
                          ID_EX(144) <= Branch;
                          ID_EX(143) <= MemRead;
                          ID_EX(142) <= MemWrite;
                          ID_EX(141) <= RegDst;
                          ID_EX(140) <= AluSrc;
                          ID_EX(139 downto 138) <= AluOp;

                          ID_EX(137 downto 106) <= id_PC;
                          ID_EX(105 downto 74)  <= id_register_rs;
                          ID_EX(73 downto 42)   <= id_register_rt;
                          ID_EX(41 downto 10)   <= id_Sign_extend;
                          ID_EX(9 downto 5)     <= id_wreg_rt;
                          ID_EX(4 downto 0)     <= id_wreg_rd;

			 end if;

                        END PROCESS;

  --------------------------------------------------------------------------
  -- execute stage
  --------------------------------------------------------------------------
 -- Declare local signals to make it easier to understand and debug (but more verbose). In
 -- practice you actually use the pipeline register bits directly but this
 -- makes the code harder to read and debug. Local signals are
 -- prefixed with the stage - here it is "id". Reference the documentation and
 -- Figure 4.46 for the meaning of bits on each pipeline register.
 --
 --Nogte that signals that are unique to EX (such as Ainput) do not need or
 --have an ex_ prefix.

         ex_register_rs <= ID_EX(105 downto 74); -- contents of register rs
         ex_register_rt <= ID_EX(73 downto 42); -- contents of register rt
         ex_PC4 <= ID_EX(137 downto 106); -- PC+4 asscoaited with the instr in
                                          -- this stage
         ex_sign_extend <= ID_EX( 41 downto 10);
         -- grab the local control bits from the pipeline register
         ex_RegDst <= ID_EX(141);
         ex_ALUSrc <= ID_EX(140);
         ex_ALUOp <= ID_EX(139 downto 138);

         ex_wreg_rt <= ID_EX(9 downto 5);
         ex_wreg_rd <= ID_EX(4 downto 0);
         --new--
         ex_reg_num_rs <= ID_EX(151 downto 147);
         ex_MemRead <=ID_EX(145);
         --new--

   -- compute the two ALU inputs
   --new stuff--
   ----------------------------------------------------------------------------------------------------
   -------------------------- FORWARDING LOGIC FOR TWO MUX CONTROLS -----------------------------------
   ----------------------------------------------------------------------------------------------------
   ForwardA <= "01" when ((wb_RegWrite = '1' and wb_wreg_addr /= X"00000000") and (wb_wreg_addr = ex_reg_num_rs)) else --MEM HAZARD
                 "10" when ((mem_RegWrite = '1' and mem_wreg_addr /= X"00000000") and (mem_wreg_addr = ex_reg_num_rs)) else --EX HAZARD
                   "00";

   ForwardB <= "01" when ((wb_RegWrite = '1' and wb_wreg_addr /= X"00000000") and (wb_wreg_addr = ex_wreg_rt)) else --MEM HAZARD
                 "10" when ((mem_RegWrite = '1' and mem_wreg_addr /= X"00000000") and (mem_wreg_addr = ex_wreg_rt)) else --EX HAZARD
                   "00";

 	Ainput <= ex_register_rs when ForwardA = "00" else
               id_write_data when (ForwardA = "01") else
                 mem_ALUOutput when (ForwardA = "10") else
                   X"00000000";
 	-- ALU input mux. ID_EX(140) is AluSrc
 	Binput <= ex_sign_extend(31 downto 0) when ex_ALUSrc = '1' else --THIS IS WHEN YOU CHOOSE THE IMMEDIATE SIGNEXTENDED
 	           ex_register_rt when ForwardB = "00" else
                id_write_data when ForwardB = "01" else
                  mem_ALUOutput when ForwardB = "10" else
                    X"BBBBBBBB";
  ----------------------------------------------------------------------------------------------------
  --new stuff--
	 ex_Branch_address <= ex_PC4  + (ex_sign_extend(29 downto 0) & "00");

	 -- Get the function field. This will be the least significant
	 -- 6 bits of  the sign extended offset

	 Function_opcode <= ex_sign_extend(5 downto 0);

		-- Generate ALU control bits. ID_EX(139) is ALuOp(1) and
                -- ID_EX(138) is AluOp(0). This is from slide 26 in the single
                -- cycle datapath

	ALU_ctl( 0 ) <= ( Function_opcode( 0 ) OR Function_opcode( 3 ) ) AND ex_ALUOp(1);
	ALU_ctl( 1 ) <= ( NOT Function_opcode( 2 ) ) OR (NOT ex_ALUOp(1) );
	ALU_ctl( 2 ) <= ( Function_opcode( 1 ) AND ex_ALUOp(1)) OR ex_ALUOp(0);

		-- Generate Zero Flag
	ex_Zero <= '1' WHEN ( ex_ALU_result = X"00000000"  )
		         ELSE '0';

-- implement the RegDst mux in this pipeline stage, ID_EX(141) is RegDst
--
       ex_wreg_addr <= ex_wreg_rd when ex_RegDst = '1' else ex_wreg_rt;


PROCESS ( ALU_ctl, Ainput, Binput )
	BEGIN
					-- Select ALU operation
 	CASE ALU_ctl IS
						-- ALU performs ALUresult = A_input AND B_input
		WHEN "000" 	=>	ex_ALU_result 	<= Ainput AND Binput;
						-- ALU performs ALUresult = A_input OR B_input
     	        WHEN "001" 	=>	ex_ALU_result 	<= Ainput OR Binput;
						-- ALU performs ALUresult = A_input + B_input
	 	WHEN "010" 	=>	ex_ALU_result 	<= Ainput + Binput;
						-- ALU performs ?
 	 	WHEN "011" 	=>	ex_ALU_result <= X"00000000";
						-- ALU performs ?
 	 	WHEN "100" 	=>	ex_ALU_result 	<= X"00000000";
						-- ALU performs ?
 	 	WHEN "101" 	=>	ex_ALU_result 	<=  X"00000000";
						-- ALU performs ALUresult = A_input -B_input
 	 	WHEN "110" 	=>	ex_ALU_result 	<= (Ainput - Binput);
						-- ALU performs SLT
                WHEN "111" 	=>	ex_ALU_result 	<= (Ainput - Binput) ;
                                -- for debugging purposes
 	 	WHEN OTHERS	=>	ex_ALU_result 	<= X"FFFFFFFF" ;

  	END CASE;
  END PROCESS;

  -- update the EX/MEM pipeline register

    PROCESS
		BEGIN
			WAIT UNTIL (rising_edge(clk));
			IF (reset = '1') THEN
                          EX_MEM <= X"00000000000000000000000000" & "000";-- initialize pipeline register
			ELSE
                          --propagate control bits
                          EX_MEM (106 downto 102) <= ID_EX(146 downto 142);

                          -- propagate instruction related values
                          EX_MEM(101 downto 70) <= ex_branch_address;
                          EX_MEM(69)            <= ex_Zero;
                          EX_MEM(68 downto 37)  <= ex_ALU_result; -- addres or ALU result
                          EX_MEM(36 downto 5)   <= ex_register_rt; -- write data
                          EX_MEM(4 downto 0)    <= ex_wreg_addr;

			 end if;

                        END PROCESS;

  --------------------------------------------------------------------------
  -- memory stage stage
  --------------------------------------------------------------------------

   mem_ALUOutput <= EX_MEM(68 downto 37); -- this could be memory address or
                                          -- result of an rformat operation
   mem_writedata <= EX_MEM (36 downto 5); -- contents of register rt
   mem_wreg_addr <= EX_MEM(4 downto 0);  -- propagate write register address
   mem_branch <= EX_MEM(104); -- controling signal denoting that this is a
   mem_branch_address <= EX_MEM(101 downto 70);
                              -- branch instruction
   mem_Zero <= EX_MEM(69); -- propagate Zero signal from EX
   -- grab the control bits for the MEM stage from the pipeline register
   mem_MemRead <= EX_MEM(103);
   mem_MemWrite <= EX_MEM(102);
   --new--
   mem_RegWrite <= EX_MEM(106); --regWRITE NEW
   --new--


    -- memory read operation. Pick the ALU_result bits from the EX/MEM
    -- register. This will contain the memory address. Use the word address (by ignoring the lower 2 bits) and
    -- convert to an integer since data memory is declared as an array. EX_MEM
    -- (103) is the MemRead control signal
    -- The concatenation of the leading 0 is to prevent the index from being interpreted as a 2's complement number
    mem_read_data <= dram(CONV_INTEGER('0'&  mem_ALUOutput(4 downto 2))) when mem_MemRead = '1'
           else X"FFFFFFFF";

    -- memory write operation. Pick the 32-bits from pipeline register EX/MEM
    -- that corresond to the contents of register rt. The LHS has the output of
    -- the ALU which is the memory address. EX_MEM (102) is the MemWrite control signal
    -- The concatenation of the leading 0 is to prevent the index from being interpreted as a 2's complement number
    dram(CONV_INTEGER('0'&  mem_ALUOutput(4 downto 2))) <= mem_writedata when  mem_MemWrite = '1'
                                                else dram(CONV_INTEGER('0'& mem_ALUOutput(4 downto 2)));

     mem_PCSrc <= mem_branch and mem_Zero; -- if (Branch and Zero) then branch


    -- update the MEM_WB pipeline register

    PROCESS
		BEGIN
			WAIT UNTIL (rising_edge(clk));
			IF (reset = '1') THEN
                          MEM_WB <= X"00000000000000000" & "000";-- initialize pipeline register
			ELSE
                          MEM_WB(70 downto 69) <= EX_MEM(106 downto 105);--
                                                                         --control bits
                          MEM_WB(68 downto 37) <= mem_read_data;
                          MEM_WB(36 downto 5 ) <= mem_ALUOutput; -- get
                                                                       -- ALU Result
                          MEM_WB(4 downto 0 ) <= mem_wreg_addr; --
                                                                     --propagate
                                                                     --write
                                                                     --reg addr
			 end if;

                        END PROCESS;


--------------------------------------------------------------------------
  -- WB stage
  --
  --------------------------------------------------------------------------
-- pull the signals from the pipeline register. Nothing is computed in this
-- stage. These signals are routed to ID where the MemToReg mux and the RegDst
-- mux is implemented
--
                        wb_RegWrite <= MEM_WB(70);
                        wb_MemToReg <= MEM_WB(69);
                        wb_read_data <= MEM_WB(68 downto 37);
                        wb_ALUOutput <= MEM_WB (36 downto 5);
                        wb_wreg_addr <= MEM_WB(4 downto 0);



--------------------------------------------------------------------------
  -- controller module
  --------------------------------------------------------------------------
--
-- recognize opcode for each instruction type
-- these variable should be inferred as wires. Note that the opcode is the
-- upper 6 bits of the IF/ID register. To avoid false hit on R-format when the system is initialized
-- i.e., when initial values in pipeline regsiters are all 0, check the function field to determine if it is really
-- an r-format instruction.

	rformat     <=  '1'  WHEN  ((IF_ID(31 downto 26) = "000000") and (IF_ID(5 downto 0) /= "000000" )) ELSE '0';
	Lw          <=  '1'  WHEN  IF_ID(31 downto 26) = "100011"  ELSE '0';
 	Sw          <=  '1'  WHEN  IF_ID(31 downto 26) = "101011"  ELSE '0';
   	Beq         <=  '1'  WHEN  IF_ID(31 downto 26) = "000100"  ELSE '0';

--
-- implement each output signal as the column of the truth
-- table  which defines the control
--

RegWrite <= (rformat or lw); -- These control the WB stage
MemToReg <= lw ;

Branch <= beq;   --These control the MEM stage
MemRead <= lw ;
MemWrite <= sw;

RegDst <= rformat;  -- These control the execute stage
ALUSrc <= (lw or sw) ;
ALUOp(1 downto 0) <=  rformat & beq; -- note the use of the concatenation operator
				     -- to form  2 bit signal



end behavioral;
