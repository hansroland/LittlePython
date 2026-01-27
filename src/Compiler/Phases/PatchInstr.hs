
module Compiler.Phases.PatchInstr where 

import Compiler.Syntax

patchInstr :: ProgAsmI -> ProgAsmI 
patchInstr (ProgAsmI frameSize instrs ) = ProgAsmI frameSize $ concat $ patchInstrInstr <$> instrs 

-- | Patch instructions. Remove illegal instructions
--    Avoid 2 memory locations in one instruction
--    Avoid immediate numbers bigger the  2^31 or smaller than -(2^31) in one instruction
patchInstrInstr :: InstrInt -> [InstrInt] 
-- Avoid 2 memory locations in one instruction
patchInstrInstr (Instr2 opc (IMem os rs) (IMem od dr)) = 
                        [Instr2 Movq (IMem os rs) (IReg Rax), 
                        Instr2 opc (IReg Rax) (IMem od dr)]
-- Avoid immediate numbers bigger the  2^31 or smaller than -(2^31) in one instruction
patchInstrInstr (Instr2 opc (IImm n) (IMem od dr)) =
                  if  (n < (-twoPower31)) ||  (n > twoPower31)
                      then  [Instr2 Movq (IImm n) (IReg Rax),
                            Instr2 opc (IMem od dr) (IReg Rax)]
                      else [Instr2 opc (IImm n) (IMem od dr)]
                where twoPower31 = (2:: Int) ^ (31:: Int) 
-- Process parameters for Call Instruction 
patchInstrInstr (InstrCall fn mbv args) = 
      concat [parmInstrs args, callInstr, assignInstr mbv] -- TODO  This is slow !! use the Shows trick. 
  where 
    parmInstrs :: [AsmIOp] -> [InstrInt]
    parmInstrs asmOps = 
        buildParm <$> zip asmOps argumentPassingRegs

    buildParm :: (AsmIOp, AsmIOp) -> InstrInt 
    buildParm (oprnd, reg) = Instr2 Movq oprnd reg

    callInstr :: [InstrInt]
    callInstr = [InstrCall fn Nothing []]  

    assignInstr :: Maybe AsmIOp -> [InstrInt]
    assignInstr (Just ivar) = [Instr2 Movq (IReg Rax) ivar ]
    assignInstr Nothing     = []
-- Don't touch all the rest of our instructions
patchInstrInstr instr = [instr]
 