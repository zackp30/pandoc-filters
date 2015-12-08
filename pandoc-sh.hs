{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import System.Process
import System.Posix.Temp
import System.Posix.Files
import System.IO
import Data.Maybe

includeDot :: Block -> IO Block
includeDot (CodeBlock (id, cls, attrs) code)
  | "sh" `elem` cls
  = do
  (path, hndl) <- mkstemp "/tmp/pandoc-z-sh"
  hPutStr hndl code
  hClose hndl
  out <- readProcess cmd [path] ""
  setFileMode path ownerExecuteMode
  return $ Plain [Str out]
  where
    cmd = fromMaybe "bash" $ lookup "bashcmd" attrs
    title = fromMaybe "" $ lookup "title" attrs

includeDot x = return x

main :: IO ()
main = toJSONFilter includeDot
