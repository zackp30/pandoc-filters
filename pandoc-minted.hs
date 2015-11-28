import Data.Maybe
import Text.Pandoc.JSON
import Text.Pandoc.Generic

-- Based on https://github.com/ncaq/pandoc-minted/blob/master/src/Main.hs
mintedBlock :: Block -> Block
mintedBlock  (CodeBlock (id, cls, attrs) code) =
    RawBlock (Format "latex") $ unlines
        [ "\\bgroup"
        , "\\begin{minted}[breakautoindent=false, breaklines=true, linenos=true]{" ++ lang ++ "}"
        , code
        , "\\end{minted}"
        , "\\captionof{listing}{" ++ caption ++ label ++ "}"
        , "\\egroup" ]
    where label = if id /= "" then "\\label{" ++ id ++  "}" else ""
          lang = if cls /= [] then head cls else "text"
          caption = fromMaybe "" $ lookup "caption" attrs
mintedBlock x = x

mintedInline :: Inline -> Inline
mintedInline (Code (_, cls, _) contents) =
  RawInline (Format "latex") $ "\\mintinline{" ++ lang ++ "}{" ++ contents ++ "}"
  where lang = if cls /= [] then head cls else "text"

mintedInline x = x

minted :: Block -> Block
minted = topDown mintedBlock . topDown mintedInline

main :: IO ()
main = toJSONFilter minted

