-- Patch instructions. Remove illegal instructions
--    Avoid 2 memory locations in one instruction
--    Avoid immediate numbers bigger the  2^31 or smaller than -(2^31) in one instruction

module Compiler.Phases.PatchInstr where 

import Compiler.Syntax

patchInstr :: ProgAsmI -> ProgAsmI 
patchInstr (ProgAsmI frameSize instrs ) = ProgAsmI frameSize $ concat $ patchInstrInstr <$> instrs 

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
patchInstrInstr instr = [instr]
 