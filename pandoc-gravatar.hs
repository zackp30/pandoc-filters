{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import Network.Gravatar
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T



includeGravatar :: Inline -> IO Inline
includeGravatar (Link attr inlines (tgt, title))
  | tgt =~ C.unpack "^user:"
  = return $ Image attr inlines (gravatar def (concatMap (\c -> )), title)

includeGravatar x = return x


main :: IO ()
main = toJSONFilter includeGravatar
