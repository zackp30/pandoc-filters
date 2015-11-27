#!/usr/bin/env runghc
{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Pandoc.JSON
import System.Process
import System.Posix.Temp
import System.IO
import Data.Maybe

includeDot :: Block -> IO Block
includeDot (CodeBlock (id, cls, attrs) code)
  | "dot" `elem` cls
  = do
  (path, hndl) <- mkstemp "/tmp/pandoc-z-dot"
  hPutStr hndl code
  hClose hndl
  callCommand $ cmd ++ path
  return $ Plain [Image ("", ["graphviz"], [("", "")]) [Str txt] (path ++ ".png", title)]
  where
    cmd = fromMaybe "dot -Tpng -O " $ lookup "dotcmd" attrs
    title = fromMaybe "" $ lookup "title" attrs
    txt = fromMaybe "" $ lookup "dotcmd" attrs

includeDot x = return x

main :: IO ()
main = toJSONFilter includeDot
