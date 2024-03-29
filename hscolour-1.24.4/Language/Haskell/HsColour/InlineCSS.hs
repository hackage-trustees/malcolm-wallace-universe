-- | Formats Haskell source code as HTML with inline CSS.
module Language.Haskell.HsColour.InlineCSS (hscolour,top'n'tail) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour.HTML (renderAnchors, renderComment,
                                       renderNewLinesAnchors, escape)
import Text.Printf

-- | Formats Haskell source code as a complete HTML document with inline styling
hscolour :: ColourPrefs -- ^ Preferences for styling.
         -> Bool   -- ^ Whether to include anchors.
         -> Int    -- ^ Starting line number (for line anchors).
         -> String -- ^ Haskell source code.
         -> String -- ^ An HTML document containing the coloured
                   --   Haskell source code.
hscolour prefs anchor n =
  pre
  . (if anchor
        then renderNewLinesAnchors n
             . concatMap (renderAnchors (renderToken prefs))
             . insertAnchors
        else concatMap (renderToken prefs))
  . tokenise

top'n'tail :: String -> String -> String
top'n'tail title  = (cssPrefix title ++) . (++cssSuffix)

pre :: String -> String
pre =   ("<pre style=\"font-family:Consolas, Monaco, Monospace;\">"++)
      . (++"</pre>")

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken prefs (cls,text) =
  stylise (colourise prefs cls) $
  if cls == Comment then renderComment text else escape text

stylise :: [Highlight] -> String -> String
stylise hs s = "<span style=\"" ++ concatMap style hs ++ "\">" ++s++ "</span>"

cssPrefix title = unlines
    ["<?xml version=\"1.0\" encoding=\"UTF-8\">"
    ,"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html>"
    ,"<head>"
    ,"<!-- Generated by HsColour, http://code.haskell.org/~malcolm/hscolour/ -->"
    ,"<title>"++title++"</title>"
    ,"</head>"
    ,"<body style=\"background-color: #131313; color: #ffffff;\">"
    ]

cssSuffix = unlines
    ["</body>"
    ,"</html>"
    ]

style :: Highlight -> String
style Normal         = ""
style Bold           = "font-weight: bold;"
style Dim            = "font-weight: lighter;"
style Underscore     = "text-decoration: underline;"
style Blink          = "text-decoration:  blink;"
style ReverseVideo   = ""
style Concealed      = "text-decoration:  line-through;"
style (Foreground c) = "color: "++csscolour c++";"
style (Background c) = "background-color: "++csscolour c++";"
style Italic         = "font-style: italic;"

csscolour :: Colour -> String
csscolour Black   = "#000000"
csscolour Red     = "#ff0000"
csscolour Green   = "#00ff00"
csscolour Yellow  = "#ffff00"
csscolour Blue    = "#0000ff"
csscolour Magenta = "#ff00ff"
csscolour Cyan    = "#00ffff"
csscolour White   = "#ffffff"
csscolour (Rgb r g b) = printf "#%02x%02x%02x" r g b

