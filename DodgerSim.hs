module Main where
import Graphics.UI.WX

-- size of display
maxX, maxY :: Int
maxY = 600
maxX = 600

data Dodger = Dodger {  id          :: String,
                        position    :: Point,
                        speed       :: Point,
                        radius      :: Int
                     }


main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    dodgers <- varCreate []
    generateDodgers dodgers
    
    f <- frameFixed [] 
    
    set f [ text := "Dodger Sim", 
            bgcolor := white, 
            layout := space maxX maxY,
            on paint := doPaint dodgers
          ]
      
    -- create a timer that updates the display
    t <- timer f [interval := 100, on command := updateDisplay dodgers f]

   
    return ()
    
    
    where
    
        doPaint :: Var [Dodger] -> DC a -> Rect -> IO ()
        doPaint ds dc _ = do
            ( (Dodger id position speed radius):_ ) <- varGet ds
            circle dc position radius []
            return ()
            
        updateDisplay :: Var [Dodger] -> Frame () -> IO ()
        updateDisplay ds f = do
            varUpdate ds dodgerUpdate
            repaint f
            return ()
        
        generateDodgers :: Var [Dodger] -> IO ()
        generateDodgers ds = do
            varUpdate ds dodgerUpdate
            return ()
            
        dodgerUpdate :: [Dodger] -> [Dodger]
        dodgerUpdate [] = [(Dodger "1" (Point 100 200) (Point 30 40) 50)]
        dodgerUpdate ( (Dodger id (Point x y) speed radius):_ ) =
            [(Dodger id (Point (x+5) y) speed radius)]


        
        
       