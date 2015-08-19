{- |
  Module for plotting program results, uses Gnuplot.

  Largely lifted from the Gnuplot package from Hackage.
-}

module IO.Plot.Gnuplot
(
-- * Attributes for plots
PlotAttribute (..)

-- * Plot from file
, plotFile
) where



-- For invoking Gnuplot from the shell:
import System.Process (spawnCommand)



-- Attributes for plots.
-- | Gnuplot attributes.
data PlotAttribute =
    EPS FilePath -- ^ Causes gp to use the EPS terminal.
  | PNG FilePath -- ^ Causes gp to use the PNG terminal.
  | XDate (Maybe String) -- ^ Sets the x-axis to plot dates, optional format.
  | Grid (Maybe [String]) -- ^ Makes a grid with different attributes.
  | Key (Maybe [String]) -- ^ Makes a legend.
  | Border (Maybe [String]) -- ^ Changes the plot border.
  | XTicks (Maybe [String]) -- ^ Changes the x-axis ticks.
  | YTicks (Maybe [String]) -- ^ Changes the y-axis ticks.
  | ZTicks (Maybe [String])
  | Title String -- ^ Changes the plot title.
  | XLabel String -- ^ Changes the x-axis label.
  | YLabel String -- ^ Changes the y-axis label.
  | ZLabel String
  | XRange (String, String) -- ^ Changes the x-axis displayed range.
  | YRange (String, String) -- ^ Changes the y-axis displayed range.
  | ZRange (String, String)



-- Plot files.
-- | Plot a function/data with attributes. For example
--
-- >>> plotFile [Grid (Just ["xtics ytics lw 1"])] "file.dat" [1,2::Int]
--
-- will produce a window containing a gridded plot of columns 1 and 2
-- of the file 'file.dat.'
plotFile :: (Integral a, Show a)
         => [PlotAttribute] -- ^ Plot settings.
         -> FilePath -- ^ File to plot.
         -> [a] -- ^ Index of
         -> IO()
plotFile attrs path (idx:idxs) = do
  let attrstr = concatMap attrToString attrs
  gnuplot $ attrstr ++ "plot " ++ mkPlots idx idxs path
plotFile _ _ [] = return ()

mkPlots :: (Integral a, Show a) => a -> [a] -> FilePath -> String
mkPlots x [y] path =
  "'" ++ path ++ "' using " ++ show x ++ ":" ++ show y ++ " with lines;"
mkPlots x (y:ys) path =
  "'" ++ path ++ "' using " ++ show x ++ ":" ++ show y ++
  " with lines," ++ mkPlots x ys path
mkPlots _ _ _ = ""

-- | Run the given string as a gnuplot command.
gnuplot :: String -> IO ()
gnuplot command = do
  _ <- spawnCommand $ "gnuplot -p -e \"" ++ command ++ "\""
  return ()

-- | Stuff to a gnuplot string from an attribute.
quote :: String -> String
quote str = "'" ++ str ++ "'"

-- | Render a plot attribute as the corresponding gnuplot command string.
attrToString :: PlotAttribute -> String
attrToString (EPS filename) =
  "set terminal postscript eps;" ++
  "set output " ++ quote filename ++ ";"
attrToString (PNG filename) =
  "set terminal pngcairo size 1920,1080;" ++
  "set output " ++ quote filename ++ ";"
attrToString (XDate (Just str)) =
  "set xdata time;" ++
  "set timefmt '" ++ str ++ "';"
attrToString (XDate Nothing) =
  "set xdata time;" ++
  "set timefmt '%Y-%m-%d %H:%M:%S';"
attrToString (Grid (Just x)) = "set grid " ++ unwords x ++ ";"
attrToString (Grid Nothing) = "set nogrid" ++ ";"
attrToString (Key (Just x)) = "set key " ++ unwords x ++ ";"
attrToString (Key Nothing) = "set nokey" ++ ";"
attrToString (Border (Just x)) = "set border " ++ unwords x ++ ";"
attrToString (Border Nothing) = "set noborder" ++ ";"
attrToString (XTicks (Just x)) = "set xtics " ++ unwords x ++ ";"
attrToString (XTicks Nothing) = "set noxtics" ++ ";"
attrToString (YTicks (Just x)) = "set ytics" ++ unwords x ++ ";"
attrToString (YTicks Nothing) = "set noytics" ++ ";"
attrToString (ZTicks (Just x)) = "set ztics" ++ unwords x ++ ";"
attrToString (ZTicks Nothing) = "set noztics" ++ ";"
attrToString (Title title) = "set title " ++ quote title ++ ";"
attrToString (XLabel label) = "set xlabel " ++ quote label ++ ";"
attrToString (YLabel label) = "set ylabel " ++ quote label ++ ";"
attrToString (ZLabel label) = "set zlabel " ++ quote label
attrToString (XRange (lo,hi)) = "set xrange ['" ++ lo ++ "':'" ++ hi ++ "'];"
attrToString (YRange _) = ""
attrToString (ZRange _) = ""
