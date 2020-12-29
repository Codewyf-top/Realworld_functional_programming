module CalculatorFront where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           CalculatorBackend

main :: IO ()
main = do
    startGUI defaultConfig setup

mkButton :: String -> UI Element
mkButton title = do
    UI.button #. "button" #+ [UI.h6 # set UI.text ("\n" ++ "\8194\8194" ++title ++ "\8194\8194" ++ "\n")]

setup :: Window -> UI ()
setup window = do
    b7  <- mkButton "7"
    b8  <- mkButton "8"
    b9  <- mkButton "9"
    bp  <- mkButton "+"
    b4  <- mkButton "4"
    b5  <- mkButton "5"
    b6  <- mkButton "6"
    bm  <- mkButton "-"
    b1  <- mkButton "1"
    b2  <- mkButton "2"
    b3  <- mkButton "3"
    bt  <- mkButton "*"
    b0  <- mkButton "0"
    bpo <- mkButton "\160.\160"
    bbl <- mkButton "("
    bd  <- mkButton "/"
    br  <- mkButton "R"
    bc  <- mkButton "C"
    bbr <- mkButton ")"
    be  <- mkButton "="
    
    input <- UI.input -- # set value "0"
    let ops = column [row $ fmap return [b7, b8, b9, bp],
                      row $ fmap return [b4, b5, b6, bm],
                      row $ fmap return [b1, b2, b3, bt],
                      row $ fmap return [b0, bpo, bbl, bd],
                      row $ fmap return [br, bc, bbr, be]]

    getBody window #+ [return input, ops]

    on UI.click b7 $ const $ do
        content <- get value input
        element input # set value (content ++ "7")

    on UI.click b8 $ const $ do
        content <- get value input
        element input # set value (content ++ "8")
    
    on UI.click b9 $ const $ do
        content <- get value input
        element input # set value (content ++ "9")

    on UI.click bp $ const $ do
        content <- get value input
        element input # set value (content ++ "+")

    on UI.click b4 $ const $ do
        content <- get value input
        element input # set value (content ++ "4")

    on UI.click b5 $ const $ do
        content <- get value input
        element input # set value (content ++ "5")

    on UI.click b6 $ const $ do
        content <- get value input
        element input # set value (content ++ "6")

    on UI.click bm $ const $ do
        content <- get value input
        element input # set value (content ++ "-")

    on UI.click b1 $ const $ do
        content <- get value input
        element input # set value (content ++ "1")

    on UI.click b2 $ const $ do
        content <- get value input
        element input # set value (content ++ "2")

    on UI.click b3 $ const $ do
        content <- get value input
        element input # set value (content ++ "3")

    on UI.click bt $ const $ do
        content <- get value input
        element input # set value (content ++ "*")
    
    on UI.click b0 $ const $ do
        content <- get value input
        element input # set value (content ++ "0")

    on UI.click bpo $ const $ do
        content <- get value input
        element input # set value (content ++ ".")

    on UI.click bbl $ const $ do
        content <- get value input
        element input # set value (content ++ "(")

    on UI.click bd $ const $ do
        content <- get value input
        element input # set value (content ++ "/")

    on UI.click br $ const $ do
        element input # set value ""

    on UI.click bc $ const $ do
        content <- get value input
        if null content
            then element input # set value ""
            else element input # set value (init content)

    on UI.click bbr $ const $ do
        content <- get value input
        element input # set value (content ++ ")")

    on UI.click be $ const $ do
        content <- get value input
        let res = calc content
        element input # set value res

