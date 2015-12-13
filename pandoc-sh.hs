{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import Text.Pandoc.Generic

import System.Process
import System.Posix.Temp
import System.Posix.Files
import System.IO

import Data.Maybe
import Control.Monad

includeShellBlock :: Block -> IO Block
includeShellBlock (CodeBlock (id, cls, attrs) code)
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

includeShellBlock x = return x

includeShellInline :: Block -> IO Block
includeShellInline (Para [Code (id, cls, attrs) code])
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

includeShellInline x = return $ x

main :: IO ()
main = toJSONFilter (bottomUpM includeShellInline <=< includeShellBlock)
