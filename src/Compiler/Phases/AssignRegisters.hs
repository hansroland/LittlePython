-- {-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Compiler.Phases.AssignRegisters (
  assignRegisters, regs0vars, regs4vars, 
  uncoverLive, edgePairs, wOps, rOps, colorGraph
  ) where 

import           Compiler.Syntax 

import           Data.Set (Set)
-- import qualified Data.Set as Set
import qualified Algebra.Graph.Undirected as G 

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Set as Set
import           Data.Maybe (isNothing, mapMaybe)
import           Data.List (nub, maximumBy, unfoldr, intercalate)
import           Data.Ord (comparing)

-- Variables for the coloring algo 

newtype Color = Color { uncolor :: Int }
    deriving (Eq, Show, Num, Ord, Enum)
    -- for conversion use: `uncolor`, or type annotation `:: Color`

instance PP Color where 
  pp (Color n) = show n

data NodeData = NodeData
    { ndId :: !AsmVOp                  -- NodeData Id = the location (AsmVOP) of the node
    , ndColor :: !(Maybe Color)        -- The color assigned to this node
    , ndMarks :: ![Color]              -- Colored neighbours ("paper marks") to calculate saturation
    }  -- deriving (Show) 

instance PP NodeData where 
    pp node = concat [ "Node: ",  show (ndId node) 
                     , " color:", pp  (ndColor node)
                     , " Marks: ",intercalate ", " (pp <$> ndMarks node)
                     ]

-- | Interference Graph with all the variables
type Graph = G.Graph AsmVOp

-- | NodeMap: Map the nodes of the graph with teh NodeData
--        key: AsmVOp 
--        value: NodeData
type NodeMap = Map AsmVOp NodeData

-- Asociation datatypes: We use them to avoid nested tuples 
-- in the unfoldr function

-- Association (AsmVOp -> Color)
data AssocVarColor = AssocVarColor AsmVOp Color
  deriving (Eq, Show) 

-- | Unwrap AssocVarColor to (AsmVOp, Color) 
unAssocVarColor :: AssocVarColor -> (AsmVOp, Color)
unAssocVarColor (AssocVarColor oprnd col) = (oprnd, col) 

-- replColorByReg :: [(AsmVOP, Int)] ->

instance PP AssocVarColor where 
  pp (AssocVarColor operand col) = 
    concat $ [pp operand, "->", pp col] 

-- | Combine the Graph with the Node data
--    The graph is constant over the whole processing
--    The NodeData changes in every unfold step
data AllGraphData = AllGraphData Graph NodeMap

-- Register Lists 

-- | Registers used for variable assignement in phase register allocation
regs4vars :: [Reg]
regs4vars = [Rcx, Rdx, Rsi, Rdi, R8, R9, R10, Rbx, R12, R13, R14]

-- | Registers NOT used for variable assignment in phase register allocation
regs0vars :: [Reg] 
regs0vars = [R15, R11, Rbp, Rsp, Rax]

-- | Calculate the variables to be store in registers
assignRegisters :: [InstrVar] ->  Map AsmVOp AsmVOp
assignRegisters vinstrs = 
  let 
    -- Create the edges of our graph
    edges = edgePairs vinstrs
    -- Get all the variables of the program  (apply nub to short lists...) 
    vars = nub $ (nub (filter isVVar (fst <$> edges)))
              <> (nub (filter isVVar (snd <$> edges)))
    -- Create a dictionary Colors -> available Registers
    colors = [(0::Color)..]
    colRegMap = Map.fromList $ zip colors regs4vars  
  in 
     -- Color the graph an 
     -- return a map variables -> registers for variables to be
     --   replace by registers
    Map.fromList $ color2Reg colRegMap <$> colorGraph edges vars

-- | Create a map from colors to the replacement registers 
color2Reg :: Map Color Reg -> (AsmVOp, Color) -> (AsmVOp, AsmVOp)
color2Reg colRegMap (vop, col) = 
    case Map.lookup col colRegMap of 
          Just reg -> (vop, VReg reg)   -- We found the color in the map -> use reg
          Nothing  -> (vop, vop)        -- otherwise keep current variable


-- Uncover Live variables

-- | uncoverLive: Performs liveness analysis.
-- Discovers which variables are in use in different regions of a program. 
-- A variable or register is live at a program point if its current value 
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
rOps (InstrCall _fn args) = Set.fromList args
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
wOps (InstrCall _s _ar) = Set.empty   -- Set.fromList $ VReg <$> calleRSavedRegs
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
    -- Here we removed the special handling of the move instruction.
    -- Example prog05.lpy didn't work. (A register was overwritten!!)
    -- example book44, however, now uses more registers
    edgePairsByInstr :: (InstrVar, Set AsmVOp) -> [(AsmVOp, AsmVOp)]
    edgePairsByInstr (instr, lafter) =          
      [(d,v) | d <- Set.toList $ wOps instr, v <- Set.toList lafter,  d /= v] 


-- Graph Coloring 
-- | Color the graph with the DSATUR graph coloring algorithm
-- This is an unfold!
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
--             b :: AllGraphData = Graph and NodeMap
--             a :: AssocVarColor
colorGraph :: [(AsmVOp, AsmVOp)] -> [AsmVOp]  -> [(AsmVOp, Color)]
colorGraph edges vars  = 
  let 
    -- Create the Graph structure
    graph = G.edges edges
    -- Create the additional info for the graph nodes
    nodeDatas = initNodes regs0vars regs4vars vars
    -- Create the nodeMap to access the nodeData by the graph nodes
    nodeMap = Map.fromList $ zip (ndId <$> nodeDatas) nodeDatas
    -- Packup all graph data in one value to use in unfoldr 
    graphData = AllGraphData graph nodeMap  
    -- Run the dsatur algo 
    assocs = unfoldr unfoldStep graphData 
  in  -- Return the result as a list (AsmVOP, Int) 
    unAssocVarColor <$> assocs 

-- | Inititialize the node datastructure
initNodes :: [Reg] -> [Reg] -> [AsmVOp] -> [NodeData]
initNodes r0vars r4vars vars = initRegNodes <> initVarNodes
  where 
    -- | Helper function to initialize the node data 
  initNode :: (AsmVOp, Maybe Color) -> NodeData 
  initNode (loc, mbColor ) = NodeData { ndId = loc, ndColor = mbColor, ndMarks = []}
  -- | initialize the register nodes, Registers have predefined colors
  initRegNodes :: [NodeData]
  initRegNodes = initNode <$>  (zip regs (Just <$> [(Color (-length r0vars))..]))
    where regs = VReg <$> r0vars <> r4vars
  -- | initialize the variable nodes from the list of variables. Variables have no colors
  initVarNodes :: [NodeData]
  initVarNodes = initNode <$> (zip vars (repeat Nothing))

-- | Color one single node of the graph
unfoldStep :: AllGraphData -> Maybe (AssocVarColor, AllGraphData)
unfoldStep (AllGraphData graph nodeMap) = 
  let 
    -- get all uncolored node keys
    keysUncolored = [ndId node | node <- Map.elems nodeMap, isNothing (ndColor node) ]
    -- | Function to calculate the saturation for a single node
    saturation :: AsmVOp -> (Int, AsmVOp) 
    saturation aNode = 
      let 
        -- Get the numbers of colored neighbours nodes 
        neighbrsData = neighboursData graph nodeMap aNode
        sat = foldr (+) 0 $ (length . ndMarks) <$> neighbrsData
      in 
        (sat,aNode)
    -- Calculate the saturation [(Int, AsmVOp)]) for all uncolored nodes
    satVals = saturation <$> keysUncolored
    nodeMaxSat = snd $ maximumBy (comparing fst) (reverse satVals)   -- TODO remove reverse
  in 
    if null keysUncolored
        then Nothing 
        else unfoldNode graph nodeMap nodeMaxSat

unfoldNode :: Graph -> NodeMap -> AsmVOp -> Maybe (AssocVarColor, AllGraphData)                
unfoldNode graph nodeMap nodeKey = 
  let 
    neighbrsData = neighboursData graph nodeMap nodeKey
    newColor = calcNewColor graph nodeMap nodeKey
    assocVarColor = AssocVarColor nodeKey newColor 
    newMap1 = updateMarks newColor neighbrsData nodeMap 
    newMap2 = updateColor newColor nodeKey newMap1 
    allGraphData = AllGraphData graph newMap2
  in 
    Just (assocVarColor, allGraphData)  

-- | NodeDatas for all the neighbours of a Node
neighboursData :: Graph -> NodeMap -> AsmVOp -> [NodeData]
neighboursData graph nodeMap aNode = 
  let 
    neighbrsKeys = Set.toList $ G.neighbours aNode graph 
    flippedLookup = flip Map.lookup
  in
    mapMaybe (flippedLookup nodeMap) neighbrsKeys

-- | Calculate the color for a node: take the lowest color
--     that is different from all the colors of the neighbours
calcNewColor :: Graph -> NodeMap -> AsmVOp -> Color 
calcNewColor graph nodeMap aNodeKey = 
  let
    neighbrsData = neighboursData graph nodeMap aNodeKey
    neighbrsColors = Set.fromList $ concat (ndMarks <$> neighbrsData)
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

-- | Add the color ouf our node to the marks of its neighbours
updateMarks :: Color -> [NodeData] -> NodeMap -> NodeMap
updateMarks col nodes nodeMap = 
  let 
    updMarks :: NodeData -> NodeData 
    updMarks nd = nd { ndMarks = col : ndMarks nd }
    updateNodeData :: NodeData -> NodeMap -> NodeMap 
    updateNodeData nd ndMap = Map.adjust updMarks (ndId nd) ndMap
  in
    foldr updateNodeData nodeMap nodes

