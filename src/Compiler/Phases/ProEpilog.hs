module Compiler.Phases.ProEpilog where 

import Compiler.Syntax 

-- | Add the prolog instructions at the beginning of the program 
--     and the epilog instructions at end of the program
-- frame 
proEpilog :: ProgAsmI -> [AsmIOp] -> ProgAsmI 
proEpilog (ProgAsmI frame instrs) usedCalleeSaveRegs = 
    -- let frm16 = Offset $ 16 * ceiling (fromIntegral (unoffset frame) / 16::Double)
    -- round up to next integral value divisible by 16
    let 
        frm16 = ((div (frame + (16 - 1))) 16) * 16  
        (saveRegs, restoreRegs) =  calleeSaveRegs usedCalleeSaveRegs  
    in ProgAsmI frm16 $ concat 
        [ prolog frm16
        , saveRegs
        , instrs 
        , restoreRegs
        , epilog frm16
        ]

-- | Prolog instructions
prolog :: Offset -> [InstrInt] 
prolog frm16 = 
    [ InstrGlob "main"
    , InstrLabl "main"
    , Instr1 Pushq (IReg Rbp)
    , Instr2 Movq (IReg Rsp)(IReg Rbp)
    , Instr2 Subq (IImm (unoffset frm16)) (IReg Rsp)
    ]
-- | Epilog instructions
epilog :: Offset -> [InstrInt]
epilog frm16 = 
    [ Instr2 Addq (IImm (unoffset frm16)) (IReg Rsp)
    , Instr1 Popq (IReg Rbp)
    , Instr2 Movq (IImm 0) (IReg Rax)             -- set returncode to 0
    , Instr0 Retq
    ]

-- Generate the instructions to save the used callee-save registers
--   Result: (saveInstructions, restoreInstructions)
calleeSaveRegs ::  [AsmIOp] -> ([InstrInt], [InstrInt])
calleeSaveRegs usedRegsToSave = 
  let 
    startOffset = -8
    pairs = zip usedRegsToSave [startOffset, startOffset-8..]
    saveInstr (reg, offset) = Instr2 Movq reg (IMem offset Rbp)
    restInstr (reg, offset) = Instr2 Movq (IMem offset Rbp) reg 
  in 
    (saveInstr <$> pairs, restInstr <$> pairs)
