{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import Network.Gravatar
import Text.Regex.Posix

default Text
includeGravatar :: Inline -> IO Inline
includeGravatar (Link attr inlines (tgt, title))
  | tgt =~ "^user:"
  = return $ Image attr inlines (gravatar def "zack@apertron.net", title)

includeGravatar x = return x


main :: IO ()
main = toJSONFilter includeGravatar
