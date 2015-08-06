{- |
  Render the statistics from a power flow solution.
-}
module IO.Render.Statistics.PowerFlow
(
-- * Rendering Power Flow Results
  renderPF
)
where



-- Electrical types:
import Util.Types

-- Rendering utilities:
import IO.Render.Util

-- The rendered data structures:
import Statistics.PowerFlow



-- | Render the results of a power flow analysis as pretty print.
renderPF :: Power -> Renderer PFStats
renderPF base (PFStats n bs ls) =
     "Power flow converged in " <> toBS n <> " iterations" <> endl
  <> "Bus summary:" <> endl
  <> busHeader <> endl
  <> foldMap (\b -> renderBus base b <> endl) bs
  <> endl
  <> "Line summary:" <> endl
  <> lineHeader <> endl
  <> foldMap (\l -> renderLine base l <> endl) ls
  <> pack (printf "%60s %8.3f  %8.3f" ("Total: "::String)
      (realPart s) (imagPart s))
  <> endl
  where
    s = foldl (\x (LineFlow _ _ _ sf st) -> x+(base:+0) * (sf+st)) 0 ls

-- | Render the headers for the power flow buses.
busHeader :: ByteString
busHeader =
     pack (printf "%5s  %18s  %18s"
      ("Bus "::String) ("Voltage      "::String) ("Power       "::String))
  <> endl
  <> pack (printf "%5s  %8s  %8s  %8s  %8s"
      ("#"::String) ("Mag(pu)"::String) ("Ang(deg)"::String)
      (" P (MW) "::String) ("Q (MVAr)"::String))

-- | Render the bus result data.
renderBus :: Power -> Renderer BusFlow
renderBus base (BusFlow i  v (p :+ q)) =
  pack $ printf "%5d  %8.3f  %8.3f  %8.2f  %8.2f"
    i (magnitude v) (toDeg $ phase v) (p * base) (q * base)

-- | Render the headers for the power flow lines.
lineHeader :: ByteString
lineHeader =
     pack (printf "%5s  %5s  %5s  %18s  %18s  %18s"
     ("Line"::String) ("From"::String) ("To  "::String)
     ("From Injection"::String) ("To Injection"::String) ("Loss    "::String))
  <> endl
  <> pack (printf "%5s  %5s  %5s  %8s  %8s  %8s  %8s  %8s  %8s"
     ("# "::String) ("Bus "::String) ("Bus "::String) ("P (MW) "::String)
     ("Q (MVAr)"::String) ("P (MW) "::String) ("Q (MVAr)"::String)
     ("P (MW) "::String) ("Q (MVAr)"::String))

-- | Render the line results data.
renderLine :: Power -> Renderer LineFlow
renderLine base (LineFlow i f t (pf:+qf) (pt:+qt)) =
  pack (printf "%5d  %5d  %5d  %8.2f  %8.2f  %8.2f  %8.2f  %8.3f  %8.3f"
    i f t bpf bqf bpt bqt bpl bql)
  where
    bpf = base * pf
    bqf = base * qf
    bpt = base * pt
    bqt = base * qt
    bpl = bpf + bpt
    bql = bqf + bqt
