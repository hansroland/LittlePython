module Compiler.Phases.ProEpilog where 

import Compiler.Syntax 

-- | Add the prolog instructions at the beginning of the program 
--     and the epilog instructions at end of the program
proEpilog :: ProgAsmI -> ProgAsmI 
proEpilog (ProgAsmI frame instrs) = 
    let frm16 = 16 * ceiling (fromIntegral frame / 16::Double)
    in ProgAsmI frm16 $ concat [prolog frm16, instrs, epilog frm16]

-- | Prolog instructions
prolog :: Int -> [InstrInt] 
prolog frm16 = 
    [ InstrGlob "main"
    , InstrLabl "main"
    , Instr1 Pushq (IReg Rbp)
    , Instr2 Movq (IReg Rsp)(IReg Rbp)
    , Instr2 Subq (IImm frm16) (IReg Rsp)
    ]
-- | Epilog instructions
epilog :: Int -> [InstrInt]
epilog frm16 = 
    [ Instr2 Addq (IImm frm16) (IReg Rsp)
    , Instr1 Popq (IReg Rbp)
    , Instr2 Movq (IImm 0) (IReg Rax)             -- set returncode to 0
    , Instr0 Retq
    ]
