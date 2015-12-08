{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import System.Process
import System.Posix.Temp
import System.Process
import Data.Maybe

includeDot :: Block -> IO Block
includeDot (CodeBlock (id, cls, attrs) code)
  | "sh" `elem` cls
  = do
  (path, hndl) <- mkstemp "/tmp/pandoc-z-sh"
  hPutStr hndl code
  hClose hndl
  out <- readProcess (cmd ++ " " ++ path)
  return $ Plain [out]
  where
    cmd = fromMaybe "bash " $ lookup "dotcmd" attrs
    title = fromMaybe "" $ lookup "title" attrs
    txt = fromMaybe "" $ lookup "dotcmd" attrs

includeDot x = return x

main :: IO ()
main = toJSONFilter includeDot
