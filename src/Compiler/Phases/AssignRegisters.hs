-- {-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Compiler.Phases.AssignRegisters (
  assignRegisters, 
  uncoverLive, edgePairs, wOps, rOps, colorGraph
  ) where 

import           Compiler.Syntax 

import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Algebra.Graph.Undirected as G 

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Maybe (isNothing, mapMaybe)
import           Data.List (nub, maximumBy, unfoldr, intercalate, intersect)
import           Data.Ord (comparing)


-- Variables for the coloring algo 

newtype Color = Color { uncolor :: Int }
    deriving (Eq, Show, Num, Ord, Enum)
    -- for conversion use: `uncolor`, or type annotation `:: Color`

instance PP Color where 
  pp (Color n) = show n

instance PP (Set Color) where 
  pp cols = pp $ Set.toList cols

instance PP [Color] where 
  pp cols = intercalate ", " (pp <$> cols)

data NodeData = NodeData
    { ndId :: !AsmVOp             -- NodeData Id = the location (AsmVOP) of the node
    , ndColor :: !(Maybe Color)   -- The color assigned to this node. 
                                  --    Must be different to all stop colors
    , ndStops :: ![Color]         -- Stop colors: The color of the neighbours. 
    }  -- deriving (Show) 

instance PP NodeData where 
    pp node = concat [ "Node: ",  show (ndId node) 
                     , " color:", pp  (ndColor node)
                     , " stops: ",intercalate ", " (pp <$> ndStops node)
                     , "\n"
                     ]

-- | Interference Graph with all the variables
type Graph = G.Graph AsmVOp

-- | NodeMap: Map the nodes of the graph with teh NodeData
--        key: AsmVOp 
--        value: NodeData
type NodeMap = Map AsmVOp NodeData

instance PP (AsmVOp, NodeData) where 
  pp (key, nodeData) = pp key <> " --> " <> pp nodeData

instance PP [(AsmVOp, Color)] where 
   pp (a : as) = pp a <> " " <> pp as
   pp []       = ""


-- Asociation datatype: We use it to avoid nested tuples 
-- in the unfoldr function

-- Association (AsmVOp -> Color)
data AssocVarColor = AssocVarColor AsmVOp Color
  deriving (Eq, Show) 

-- | Unwrap AssocVarColor to (AsmVOp, Color) 
unAssocVarColor :: AssocVarColor -> (AsmVOp, Color)
unAssocVarColor (AssocVarColor oprnd col) = (oprnd, col) 

instance PP AssocVarColor where 
  pp (AssocVarColor operand col) = 
    concat $ [pp operand, "->", pp col] 

-- | Calculate the variables to be store in registers
--    and the usedCallee Registers
-- A programs with less than 3 variables in its graph is too small to be 
-- useful for the algo. But programs with less than 3 variables don't need
-- any optimization !
assignRegisters :: [InstrVar] ->  (Map AsmVOp AsmIOp, [AsmIOp])
assignRegisters vinstrs = 
  let 
    -- Create the edges of our graph
    edges = edgePairs vinstrs
    -- Get all the variables (and caller saved regs) of the program  
    --    (apply nub to shorten the lists...) 
    varsOrRegs = nub $ (nub (filter isVVarOrVReg (fst <$> edges)))
                    <> (nub (filter isVVarOrVReg (snd <$> edges)))
    vars = filter isVVar varsOrRegs
    regs = filter isVReg varsOrRegs
    -- Create a map: Colors -> available Registers
    colRegMap = Map.fromList $ zip [(0::Color)..] reg4varsOpnds 
    -- Create a map: avaliable Registers -> Color
    regColMap = Map.fromList $ zip reg4varsOpnds [(0::Color)..] 
    -- Assign colors to used registers
    regColPairs = [(r,c) | r <- regs, let c = (Map.!) regColMap r]
    
    -- Color the graph so that direct neighbours don't have the same color
    -- return a map variables -> registers for variables to be
    --   replace by registers
    colorResult = colorGraph edges vars regColPairs
    varmap = Map.fromList $ concat $ color2Reg colRegMap <$> colorResult
    usedCalleeRegs = nub $ (IReg <$> calleESavedRegs) `intersect` (filter isIReg (Map.elems varmap))
  in 
    (varmap, usedCalleeRegs)

-- | Create a map from colors to replace variables by registers 
-- | Switch from asmVOp to asmIOp data types
color2Reg :: Map Color AsmVOp -> (AsmVOp, Color) -> [(AsmVOp, AsmIOp)]
color2Reg colRegMap (vop, col) = 
    case Map.lookup col colRegMap of 
          Just (VReg reg) -> [(vop, IReg reg)]   -- We found the color in the map -> use reg
          _               -> []                  -- otherwise, ignore it


-- Uncover Live variables

-- | uncoverLive: Performs liveness analysis.
-- Discovers which variables are in use in different regions of a program. 
-- A variable is live at a program point if its current value 
-- is used at some later point in the program.
uncoverLive :: [InstrVar] -> [(InstrVar, Set AsmVOp)] 
uncoverLive insts = zip insts $ reverse $ scanl step Set.empty $ reverse insts 
  where
    step :: Set AsmVOp -> InstrVar -> Set AsmVOp
    step lafter inst = 
        --  L before (k) = (L after (k) − W(k)) ∪ R(k),
        (lafter `Set.difference` (wOps inst)) `Set.union` (rOps inst)

-- | Extract the read operands from an instruction 
-- The callq instruction should include the appropriate 
--   argument-passing registers in its read set R, 
--   depending on the arity of the function being called.
rOps :: InstrVar -> Set AsmVOp
-- Movq doesn't read the second operand
rOps (Instr2 Movq s _) = if isVImm s then Set.empty else Set.singleton s
rOps (Instr2 _op s d)  = if isVImm s then Set.singleton d else Set.fromList [s,d] 
rOps (Instr1 _op sd)   = Set.singleton sd
rOps (InstrCall _fn _as args) = Set.fromList args
rOps (Instr0 _op) = Set.empty
rOps (InstrGlob _lbl) = Set.empty
rOps (InstrLabl _lbl) = Set.empty

-- | Extract the write operands from an instruction
-- The callq instruction should include all the caller-saved 
--    registers in its write set W because the calling convention says that
--    those registers may be written to during the function call.
wOps :: InstrVar -> Set AsmVOp
wOps (Instr2 _ _ d) = Set.singleton d 
wOps (Instr1 _ sd)  = Set.singleton sd
wOps (InstrCall _fn (Just as) _ar) = Set.singleton as   
wOps (InstrCall _fn  Nothing  _ar) = Set.empty   
wOps (Instr0 _) = Set.empty
wOps (InstrGlob _) = Set.empty
wOps (InstrLabl _) = Set.empty

-- | Prepare the edges for the interference graph.
-- For each instruction, create an edge between the locations being 
-- written to and the live locations. (However, a location never 
-- interferes with itself.)
edgePairs :: [InstrVar] -> [(AsmVOp, AsmVOp)]
edgePairs instrs = nub $ concat $ edgePairsByInstr <$> uncoverLive instrs
  where
    edgePairsByInstr :: (InstrVar, Set AsmVOp) -> [(AsmVOp, AsmVOp)]
    edgePairsByInstr (Instr2 Movq s d, lafter) =
      [(d,v) | v <- Set.toList lafter, v /= d, v /= s]
    edgePairsByInstr (instr@(InstrCall _fn _opnd _args), lafter) = edgePairsStd instr lafter <>
      [(d,v) | v <- Set.toList lafter, d <- calleRSavedRegs4Vars, d /= v ]
    edgePairsByInstr (instr, lafter) = edgePairsStd instr lafter
    --
    edgePairsStd instr lafter =
      [(d,v) | d <- Set.toList $ wOps instr, v <- Set.toList lafter,  d /= v]

-- | Inititialize the node datastructure
initNodes :: [AsmVOp] -> [(AsmVOp, Color)] -> [NodeData]
initNodes vars regColPairs = initRegNodes <> initVarNodes  
  where 
    -- | Helper function to initialize the node data 
    initNode :: (AsmVOp, Maybe Color) -> NodeData 
    initNode (loc, mbColor ) = NodeData { ndId = loc, ndColor = mbColor, ndStops = []}
    -- | Create NodeDatas for the register nodes, Registers have predefined colors
    --   create only the registers of the set regs4vars
    initRegNodes :: [NodeData]
    initRegNodes = initNode <$> (fmap (fmap Just) regColPairs)
    -- | initialize the variable nodes from the list of variables. Variables have no colors
    initVarNodes :: [NodeData]
    initVarNodes = initNode <$> (zip vars (repeat Nothing))


-- Graph Coloring 
-- | Color the graph with the DSATUR graph coloring algorithm
-- This is an unfold!
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
--             b :: NodeMap
--             a :: AssocVarColor
colorGraph :: [(AsmVOp, AsmVOp)] -> [AsmVOp] -> [(AsmVOp, Color)] -> [(AsmVOp, Color)]
colorGraph edges vars regColPairs
  = 
  let 
    -- Create the Graph structure
    graph = G.edges edges
    -- Create the additional info for the graph nodes
    nodeDatas = initNodes vars regColPairs
    -- Create the nodeMap to access the nodeData by the graph nodes
    nodeMap = Map.fromList $ zip (ndId <$> nodeDatas) nodeDatas
    -- Registers already have colors
    -- Update the stop marks for the neighbours of the registers
    newNodeMap = setRegStops graph regColPairs nodeMap 
    -- Run the dsatur algo 
    assocs = unfoldr (unfoldStep graph) newNodeMap 
  in  -- Return the result as a list (AsmVOP, Int) 
    unAssocVarColor <$> assocs 

-- | Color one single node of the graph.
--    The graph is constant over the whole coloring processing
--    The NodeMap with the node data changes in every unfold step
unfoldStep :: Graph -> NodeMap -> Maybe (AssocVarColor, NodeMap)
unfoldStep graph nodeMap = 
  let 
    -- | Function to calculate the saturation for a single node
    --     The saturation of a node is the size of the set of colors, 
    --     that are no longer available to be used as colors for this nodes.
    --     We look for the node with the least number of possibile colors 
    saturation :: AsmVOp -> (Int, AsmVOp) 
    saturation aNode = 
      let 
        -- Get the number of different colors from the neighbours
        sat = length $ neighboursColors graph nodeMap aNode
      in 
        (sat,aNode)
    -- get all the yet uncolored node keys
    keysUncolored = [ndId node | node <- Map.elems nodeMap, isNothing (ndColor node) ]
    -- Calculate the saturation [(Int, AsmVOp)]) for all uncolored nodes
    satVals = saturation <$> keysUncolored
    nodeMaxSat = snd $ maximumBy (comparing fst) satVals
  in 
    if null keysUncolored
        then Nothing 
        else unfoldNode graph nodeMap nodeMaxSat

unfoldNode :: Graph -> NodeMap -> AsmVOp -> Maybe (AssocVarColor, NodeMap)                
unfoldNode graph nodeMap nodeKey = 
  let 
    neighbrsData = neighboursData graph nodeMap nodeKey
    newColor = calcNewColor nodeMap nodeKey
    assocVarColor = AssocVarColor nodeKey newColor 
    newMap1 = updateStops newColor neighbrsData nodeMap 
    newMap2 = updateColor newColor nodeKey newMap1 
  in 
    Just (assocVarColor, newMap2)  

-- | Neighbours Colors - get all the colors from the neighbours
neighboursColors :: Graph -> NodeMap -> AsmVOp -> [Color]
neighboursColors graph nodeMap nNodeKey = 
  let 
    neighboursKeys  = (Set.toList $ G.neighbours nNodeKey graph)
    neighboursNodes =  ((Map.!) nodeMap) <$> neighboursKeys
  in  
    nub $ mapMaybe ndColor neighboursNodes

-- | NodeDatas for all the neighbours of a Node
neighboursData :: Graph -> NodeMap -> AsmVOp -> [NodeData]
neighboursData graph nodeMap aNode = 
  let 
    neighbrsKeys = (Set.toList $ G.neighbours aNode graph)  
    flippedLookup = flip Map.lookup
  in
    mapMaybe (flippedLookup nodeMap) neighbrsKeys

-- | Calculate the color for a node: take the lowest color
--     that is different from all the colors of the neighbours
calcNewColor :: NodeMap -> AsmVOp -> Color 
calcNewColor nodeMap aNodeKey = 
  let
    nodeData = (Map.!) nodeMap aNodeKey 
    neighbrsColors =  Set.fromList $ ndStops nodeData
    unusedColors = Set.difference 
      (Set.fromDistinctAscList $ Color <$> [0..(Map.size nodeMap)]) 
      neighbrsColors
  in
    Set.findMin unusedColors

-- | Write back the color of our node to the dictionary 
updateColor :: Color -> AsmVOp -> NodeMap -> NodeMap
updateColor col nodeKey nodeMap = 
  let 
    updColor :: NodeData -> NodeData
    updColor nd = nd {ndColor = Just col }  
  in 
    Map.adjust updColor nodeKey nodeMap

-- | Add the color of our node to the stops of its neighbours
updateStops :: Color -> [NodeData] -> NodeMap -> NodeMap
updateStops col neigbourNodes nodeMap = 
  let 
    updStops :: NodeData -> NodeData 
    updStops nd = nd { ndStops = col : ndStops nd }
    updateNodeData :: NodeData -> NodeMap -> NodeMap 
    updateNodeData nd ndMap = Map.adjust updStops (ndId nd) ndMap
  in
    foldr updateNodeData nodeMap neigbourNodes

setRegStops :: Graph -> [(AsmVOp, Color)] -> NodeMap -> NodeMap
setRegStops graph regcols nodeMap = foldr (setRegStop graph) nodeMap regcols

setRegStop :: Graph -> (AsmVOp, Color) -> NodeMap -> NodeMap 
setRegStop graph (aNodeKey, color) nodeMap = 
  let 
    neighbrsData = neighboursData graph nodeMap aNodeKey
    -- aNode = fromJust $ Map.lookup aNodeKey nodeMap
  in  
    updateStops color neighbrsData nodeMap

-- | Registers to use for variable assignment in phase register allocation
regs4vars :: [Reg]
regs4vars = [Rcx, Rdx, Rsi, Rdi, R8, R9, R10, Rbx, R12, R13, R14 ] 

-- | Register to stop at a 'call' instruction 
calleRSavedRegs4Vars :: [AsmVOp]
calleRSavedRegs4Vars = VReg <$> (regs4vars `intersect` calleRSavedRegs)

-- | Just the registers for variable operands 
reg4varsOpnds :: [AsmVOp]
reg4varsOpnds =  VReg <$> regs4vars
