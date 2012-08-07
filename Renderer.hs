{-# LANGUAGE OverloadedStrings #-}
module Renderer (render) where

import Control.Monad (forM_)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Int -> H.Html
render cpus =
  H.docTypeHtml $ do
    H.head $ do
      H.title $ (H.toHtml title)
      H.style $ (H.toHtml style)
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-1"
      H.script ! A.type_ "text/javascript" ! A.src "js/main.js" $ ""
      H.script ! A.type_ "text/javascript" ! A.src "js/FrequencyMeter.js" $ ""
    H.body ! A.onload (toValue $ pageLoaded cpus) $ do
      H.table $ do
        forM_ list row
  where
    title = "-= CPU Frequency. (" ++ (show cpus) ++ ") CPUs =-"
    style = "body {background-color:black;}" :: String
    list  = take (cpus `Prelude.div` 2) $ [n | n <- [0..], even(n)]
    
-- A table row with two canvases on each row (CPU<N> and CPU<N + 1>)
row :: Int -> H.Html
row n =
  H.tr $ do
    H.td $ do
      H.canvas ! A.id (cpu n) $ ""
    H.td $ do
      H.canvas ! A.id (cpu $ n + 1) $ ""
  where
    cpu n = toValue $ "CPU" ++ show n
    
pageLoaded :: Int -> String
pageLoaded cpus =
  "pageLoaded(" ++ array ++ ");"
  where
    array     = fixUp . show $ Prelude.map (("CPU" ++) . show) [0..(cpus - 1)]
    fixUp str = Prelude.map replace str
    replace c 
      | c == '\"' = '\''
      | otherwise = c
