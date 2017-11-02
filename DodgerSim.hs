module Main where
import Graphics.UI.WX

main = start myGUI

myGUI :: IO ()
myGUI = do 
    f    <- frame    [text := "Frame"]
 --   set f [layout := margin 10 (column 5 [floatCentre (label "Hello")] )]
{-
    w   <- window f [text := "Window"]
    d   <- dialog w [text := "Dialog"]
    ok  <- button d [text := "Ok"]
    result <- showModal d (\stop -> set ok [on command := stop (Just 42)])
-}  
    myVar1 <- varCreate (1 :: Int)
    myVar2 <- varCreate (2 :: Int)
    
    set f [text := "Panel", on paint := panelPaint myVar1 myVar2]

    -- create a panel to draw in.
    -- p <- panel f [text := "Panel", on paint := panelPaint]
   
    return ()
    
    where
    
        panelPaint :: Var Int -> Var Int -> DC a -> Rect -> IO ()
        panelPaint vi1 vi2  dc viewArea
          = do circle dc (point 100 100) 80 []
               return ()
          
          
    
    
                                     
                                     
       