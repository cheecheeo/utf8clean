-- Copyright (C) 2012 John Chee
-- See LICENSE for more details

-- TODO verify mode, cleanup mode
-- TODO in verify mode, report _all_ failures (line + byte in line + total byte offset)

import           Control.Applicative        ((<$>))
import           Data.Char                  (GeneralCategory(..))
import           Data.Char                  as C
import           Data.Int                   (Int64)

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text.Encoding.Error   (OnDecodeError)
import qualified Data.Text.Encoding.Error   as DTEE
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy.Encoding    as DTE
import qualified Data.Text.Lazy.IO          as TIO

-- Copied from: http://hackage.haskell.org/packages/archive/text/0.11.2.3/doc/html/src/Data-Text-Lazy-IO.html#hGetContents
getContentsDecodedWith :: OnDecodeError -> IO Text
getContentsDecodedWith ode = fmap (DTE.decodeUtf8With ode) L8.getContents

interactDecodeWith :: OnDecodeError -> (Text -> Text) -> IO ()
interactDecodeWith ode f = TIO.putStr . f =<< (getContentsDecodedWith ode)

-- TODO format characters are probably valid
isWeird :: Char -> Bool
isWeird c =
  case C.generalCategory c of
    Format      -> True
    NotAssigned -> True
    PrivateUse  -> True
    Surrogate   -> True
    _           -> False

weirdCharactersWithIndices :: ByteString -> [(Int64, Char)]
weirdCharactersWithIndices bs = snd $ L8.foldr (\c (oldIndex, ics) ->
  let currentIndex = oldIndex + 1
  in (currentIndex, if isWeird c then (currentIndex, c) : ics else ics))
  (0, []) bs

main :: IO ()
--main = interactDecodeWith DTEE.lenientDecode id
main = print =<< (weirdCharactersWithIndices <$> L8.getContents)
--main = print =<< (L8.findIndices isWeird <$> L8.getContents)
--main = putStr =<< (unlines . map (show . C.generalCategory) . L8.unpack <$> L8.getContents)
