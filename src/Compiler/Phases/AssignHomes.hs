module Compiler.Phases.AssignHomes (assignHomes) where 

import Compiler.Syntax

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

emptydict :: Map String AsmVOp
emptydict = Map.empty

type AssignMonad a = State (Map String AsmVOp) a

assignHomes :: ProgAsmV -> ProgAsmV 
assignHomes (ProgAsmV _ vstmts) =
   let 
       (astmts, vardict) = runState (sequence (asHome <$> vstmts)) emptydict
       frameSize = Map.size vardict
   in ProgAsmV frameSize $ astmts 

asHome :: InstrVar -> AssignMonad InstrVar 
asHome (Instr2 opc (VVar s1) (VVar s2)) = liftA2 (Instr2 opc) (getHome s1) (getHome s2)
asHome (Instr2 opc (VVar s1) v2)        = liftA2 (Instr2 opc) (getHome s1) (pure v2) 
asHome (Instr2 opc  v1 (VVar s2))       = Instr2 opc <$> pure v1 <*> getHome s2
asHome (Instr1 opc (VVar s1))           = Instr1 opc <$> getHome s1
asHome instr                            = pure instr

getHome :: String -> AssignMonad AsmVOp 
getHome varnam = do 
    mbvdict <- get
    let mbmem = Map.lookup varnam mbvdict 
    case mbmem of 
        Nothing -> do
            let offset = -8 * (1 + Map.size mbvdict) 
            let newEntry = VMem offset Rbp
            let newdict = Map.insert varnam newEntry mbvdict
            _ <- put newdict
            return newEntry
        Just op -> return op
