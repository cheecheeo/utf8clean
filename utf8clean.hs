-- Copyright (C) 2012 John Chee
-- See LICENSE for more details

-- TODO verify mode, cleanup mode
-- TODO in verify mode, report _all_ failures (line + byte in line + total byte offset)

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as DTE
import Data.Text.Encoding.Error (OnDecodeError)
import qualified Data.Text.Encoding.Error as DTEE

-- Copied from: http://hackage.haskell.org/packages/archive/text/0.11.2.3/doc/html/src/Data-Text-Lazy-IO.html#hGetContents
getContentsDecodedWith :: OnDecodeError -> IO Text
getContentsDecodedWith ode = fmap (DTE.decodeUtf8With ode) L8.getContents

interactDecodeWith :: OnDecodeError -> (Text -> Text) -> IO ()
interactDecodeWith ode f = TIO.putStr . f =<< (getContentsDecodedWith ode)

main :: IO ()
main = interactDecodeWith DTEE.lenientDecode id
--main = interactDecodeWith DTEE.strictDecode id
