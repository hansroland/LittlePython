module Compiler.Run.Options (Settings(..), getOptions, 
    dumpUsage, options, testSettings) where  

import SimpleGetOpt 

-- | Compiler Settings from the argv vector.
data Settings = Settings
  { printVersion :: Bool      -- ^ Print the version (=compilation date) of the compiler
  , printInp     :: Bool      -- ^ Print the contents of the input file
  , printAst     :: Bool      -- ^ Print the Abstract Syntax Tree (AST)
  , printRco     :: Bool      -- ^ Print the outout of the remove complex operands phase
  , printSi      :: Bool      -- ^ Print the assembler code after Select Instructions phase
  , printAr      :: Bool      -- ^ Print the assembler code after Assign Register phase
  , printAh      :: Bool      -- ^ Print the assembler code after Assign Homes phase
  , printPatch   :: Bool      -- ^ Print the assembler code after the Patch Instructions phase 
  , printEpilog  :: Bool      -- ^ Print the assembler code after adding prolog and epilog
  , srcFile      :: String    -- ^ The input source file
  , outdir       :: String }  -- ^ The directory for the output
  deriving (Show)

-- | Parse the options string 
getOptions :: IO Settings 
getOptions =  getOpts defaultSettings options

-- | The default settings
defaultSettings :: Settings
defaultSettings = Settings
  { printVersion = False
  , printInp = False
  , printAst = False
  , printRco = False 
  , printSi  = False
  , printAr  = False
  , printAh  = False
  , printPatch = False
  , printEpilog = False
  , outdir = "./bin"
  , srcFile     = ""
  }

-- Compiler settings for the test runner
testSettings :: FilePath -> Settings
testSettings path =  defaultSettings{srcFile = path } 

-- |Definition of the available options
options :: OptSpec Settings
options = optSpec
  { progDescription = [ "A toy compiler for the LittlePython language." ]

  , progOptions =
      [ Option ['i'] ["printInp"]
        "Display the source code."
        $ NoArg $ \s -> Right s { printInp = True }

      , Option ['a'] ["printAst"]
        "Display the AST."
        $ NoArg $ \s -> Right s { printAst = True }

      , Option ['c'] ["printRco"]
        "Display after the removal of complex operation."
        $ NoArg $ \s -> Right s { printRco = True }

      , Option ['s'] ["printSi"]
        "Display the first assembler code."
        $ NoArg $ \s -> Right s { printSi = True }

      , Option ['r'] ["printAr"]
        "Display after AssignRegisters."
        $ NoArg $ \s -> Right s { printAr = True }

      , Option ['h'] ["printAh"]
        "Display after AssignHomes."
        $ NoArg $ \s -> Right s { printAh = True }

      , Option ['p'] ["printPatch"]
        "Display after Patch Instructions."
        $ NoArg $ \s -> Right s { printPatch = True }

      , Option ['e'] ["printEpilog"]
        "Display after adding Pro- and Epilog."
        $ NoArg $ \s -> Right s { printEpilog = True }

      , Option ['v'] ["version"]  
        "Display version info" 
        $ NoArg $ \s -> Right s { printVersion = True }

      , Option ['o'] ["outdir"]
        "Output directory for binaries"
        $ ReqArg "STRING" $ \a s -> 
             case null a of
              False -> Right $ s { outdir = a }
              True  -> Left "outdir is empty"
      ]
  , progParamDocs =
      [ ("FILE",   "The file source that needs processing.") ]

  , progParams = \p s -> Right s { srcFile = p }

  , progArgOrder = Permute
  }
