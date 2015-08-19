{- |
  Program configuration information. A simple way to define analyses, the grid
  file associated with it, and what should be done with them.
-}
module Natural.Config
(
-- * The config type
  Config (..)

-- * Associated types
, GridFormat (..)
, Analysis (..)
, Output (..)

-- Default and empty configurations
, defaultConfig
, emptyConfig
) where



-- | The configuration data type.
--
-- Determines what a program run entails.
data Config =
  Config
  { confGridFile :: FilePath
  , confGridFormat :: GridFormat
  , confAnalyses :: [Analysis]
  , confOutputs :: [Output]
  } deriving (Show)


-- | Types of file formats available.
data GridFormat = Simple | MCF
  deriving (Show)

-- | Types of available analyses.
data Analysis = PowerFlowGS | PowerFlowJC | PowerFlowNR | PowerFlowNJ
  deriving (Show)

-- | Types of output to generate.
data Output = GridStatistics
  deriving (Show)



-- Default and empty configs:
-- | The default analysis is Gauss-Seidel and Jacobi power flows of the
-- IEEE 14 bus standard example grid in the MatPower Case Format.
defaultConfig :: Config
defaultConfig =
  Config
  { confGridFile = "case5.m"
  , confGridFormat = MCF
  , confAnalyses = [PowerFlowGS, PowerFlowJC, PowerFlowNR, PowerFlowNJ]
  , confOutputs = [] }

-- | An empty configutation, for filling. This may turn out to be a hack.
emptyConfig :: Config
emptyConfig =
  Config
  { confGridFile = ""
  , confGridFormat = MCF
  , confAnalyses = []
  , confOutputs = []
  }
