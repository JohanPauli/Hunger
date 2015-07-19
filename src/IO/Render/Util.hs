{- |
  Utilities for rendering text files.
-}
module IO.Render.Util
(
-- * Rendering primitives
  Renderer

-- * Utilities
, sep
, endl
, renderComplex
, toBS
, renderString

-- * Assisting library stuff
, module X
) where



-- Strings:
import Data.ByteString.Char8 as X (ByteString, pack, unpack)

-- Control:
import Data.Monoid as X ((<>))

-- Folables for mapping and folding.
import Data.Foldable as X (foldMap)

-- Printf, like the one in C.
import Text.Printf as X

-- Complex numbers:
import Data.Complex




-- | A renderer type synonym. A renderer is just something which makes
-- a `ByteString` for printing.
type Renderer a = (a -> ByteString)



-- Utilities.
-- | Seperator character, for csv values.
sep :: ByteString
sep = " "

-- | End of line character.
endl :: ByteString
endl = "\n"

-- | Render a complex value (in rectangular form).
renderComplex :: (Show a, RealFloat a) => Renderer (Complex a)
renderComplex z =
     toBS (realPart z)
  <> "+"
  <> toBS (imagPart z)
  <> "i"

-- | Renders a member of `Show` as a `ByteString`.
toBS :: (Show a) => a -> ByteString
toBS = pack . show

-- | Renders a string encased in double quotes.
renderString :: Renderer String
renderString str =
     "\""
  <> pack str
  <> "\""
