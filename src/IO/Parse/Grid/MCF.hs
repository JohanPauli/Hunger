{- |
  This is used for parsing the format used by Matpower, MATLAB Case File
  format (MCF). Mostly for testing PF.
-}
module IO.Parse.Grid.MCF
(
-- * Parsing MCF cases
  parseMCF
) where



-- Electrical types:
import Util.Types

-- Parsing utilities:
import IO.Parse.Util

-- The data structure to be parsed:
import Natural.Simple



-- Parsing MCF cases:
-- | The parser recursively fills a grid object's fields; probably not
-- the best approach, but it works.
parseMCF :: Parser SGrid
parseMCF = parseField $ return emptySGrid{gridName="MCF grid"}

-- | Parses an MCF case into a `SGrid`.
parseField :: Parser SGrid -> Parser SGrid
parseField gp =
  -- Skip any space at beginning of line.
  skipSpace >>
  -- There are 5 types of relevant structure to parse.
      (string "mpc.gencost" >> skipLine >> parseField gp)
  <|> (string "mpc.baseMVA" >> skipSpace >> char '=' >> skipSpace
        >> parseBaseMVA gp)
  <|> (string "mpc.bus" >> skipWhile (/='[') >> char '[' >> parseBuses gp)
  <|> (string "mpc.gen" >> skipWhile (/='[') >> char '[' >> parseGens gp)
  <|> (string "mpc.branch" >> skipWhile (/='[') >> char '[' >> parseLines gp)
  <|> (endOfInput >> gp)
  <|> (skipLine >> parseField gp)



-- Parsing the base MVA:
-- | Parse the 'mpc.baseMVA' field of the MATLAB file.
parseBaseMVA :: Parser SGrid -> Parser SGrid
parseBaseMVA gp = do
  g <- gp
  base <- double
  skipLine
  parseField $ return g{gridMVAbase=base}



-- Parsing MCF Buses:
-- | Parse the bus field of a MCF file.
parseBuses :: Parser SGrid -> Parser SGrid
parseBuses gp =
  -- Either the structure ends, or an entry is to be parsed.
  skipSpace >>
  ( do
    _ <- string "]"
    g <- gp
    parseField $ return g{gridBuses=reverse $ gridBuses g}
  )
  <|> do
    g <- gp
    b <- parseBus
    parseBuses $ return g{gridBuses=b:gridBuses g}

-- | Parse a single bus field.
parseBus :: Parser SBus
parseBus = do
  -- First is bus ID.
  bID <- decimal
  -- Bus type skipped.
  skipVal
  -- P and Q loads.
  busP <- double
  skipSpace
  busQ <- double
  skipSpace
  -- Shunt admittance.
  gs <- double
  skipSpace
  bs <- double
  -- Skip 'area'.
  skipVal
  -- Voltage.
  vMag <- double
  skipSpace
  vAng <- double
  skipSpace
  -- Base voltage.
  baseKV <- double
  skipLine
  return SBus
    { sbusName = "An MCF bus"
    , sbusID = bID
    , sbusVoltage = mkPolar vMag (toRad vAng)
    , sbusVoltageBase = baseKV
    , sbusAdmittance = gs :+ bs
    , sbusPower = busP :+ busQ
    }



-- Generator parsing:
-- | Parse the generator field of an MCF file.
parseGens :: Parser SGrid -> Parser SGrid
parseGens gp =
  skipSpace >>
  ( do
    _ <- string "]"
    g <- gp
    parseField $ return g{gridGens=reverse $ gridGens g}
  )
  <|> do
    g <- gp
    gen <- parseGen
    parseGens $ return g{gridGens=gen:gridGens g}

-- | Parse a single generator field.
parseGen :: Parser SGen
parseGen = do
  -- Bus ID.
  bID <- decimal
  skipSpace
  -- Generator P and Q.
  p <- double
  skipSpace
  q <- double
  skipSpace
  -- Reactive limits.
  qMax <- double
  skipSpace
  qMin <- double
  skipSpace
  -- Generator voltage setpoint.
  v <- double
  -- Skip until generating limits.
  _ <- count 2 skipVal
  -- Real generating limits.
  pMax <- double
  skipSpace
  pMin <- double
  -- The rest of the values are ignored.
  skipLine
  return $ SGen bID (p :+ q) (pMax :+ qMax) (pMin :+ qMin) v



-- Parsing MCF lines.
-- | Parse the line field of a MCF file.
parseLines :: Parser SGrid -> Parser SGrid
parseLines gp =
  skipSpace >>
  ( do
    _ <- string "]"
    g <- gp
    parseField $ return g{gridLines=reverse $ gridLines g}
  )
  <|> do
    g <- gp
    let i = if null (gridLines g) then 0 else (slineID . head . gridLines) g
    l <- parseLine (i+1)
    parseLines $ return g{gridLines=l:gridLines g}

-- | Parse a single line.
parseLine :: Int -> Parser SLine
parseLine i = do
  -- Line from.
  lFrom <- decimal
  skipSpace
  -- Line to.
  lTo <- decimal
  skipSpace
  -- Line R and X
  lR <- double
  skipSpace
  lX <- double
  skipSpace
  lB <- double
  skipLine
  return $ SLine i lFrom lTo (lR :+ lX) lB
