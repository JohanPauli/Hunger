{- |
  Configuration file parser.
-}
module IO.Parse.Config
(
-- * Configuration parsing
  parseConfig
)
where



-- Parsing:
import Data.Config
import IO.Parse.Util as P8



-- | Parse a configuration file, returning a `Config`.
parseConfig :: Parser Config
parseConfig = parseField (return emptyConfig)

-- | Parse a field of the configuration file.
--
-- Fields may be empty lines, comments, or meaningful fields.
parseField :: Parser Config -> Parser Config
parseField conf =
  skipSpace >>
      (comment >> parseField conf)
  <|> (string "Grid file name:" >> parseGridFP conf)
  <|> (string "Grid file format:" >> parseGridFormat conf)
  <|> (string "Analysis:" >> parseAnalysis conf)
  <|> (string "Output:" >> parseOutput conf)
  <|> (endOfInput >> conf)
  <|> error "The configuration file has a syntax error."

-- | Parse the file path.
parseGridFP :: Parser Config -> Parser Config
parseGridFP conf = do
  skipSpace
  fp <- takeLine
  c <- conf
  parseField $ return c{confGridFile=fp}

-- | Parse the grid format.
parseGridFormat :: Parser Config -> Parser Config
parseGridFormat conf = do
  skipSpace
  fmt <- (string "Simple" >> return Simple)
     <|> (string "MCF" >> return MCF)
  skipLine
  c <- conf
  parseField $ return c{confGridFormat=fmt}

-- | Parse an analysis:
parseAnalysis :: Parser Config -> Parser Config
parseAnalysis conf = do
  skipSpace
  ana <- (string "Gauss-Seidel Power Flow" >> return PowerFlowGS)
     <|> (string "Jacobi Power Flow" >> return PowerFlowJC)
     <|> error "An 'Analysis:' field has an invalid value."
  skipLine
  c <- conf
  parseField $ return c{confAnalyses=ana:confAnalyses c}

parseOutput :: Parser Config -> Parser Config
parseOutput conf = do
  skipSpace
  op <- (string "Grid Statistics" >> return GridStatistics)
    <|> error "An 'Output:' field has an invalid value."
  c <- conf
  parseField $ return c{confOutputs=op:confOutputs c}
