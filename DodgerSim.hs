module Main where
import Graphics.UI.WX
import System.Random

-- size of display
maxX, maxY :: Int
maxY = 600
maxX = 600

data Dodger = Dodger {  id          :: String,
                        position    :: Point,
                        speed       :: Point,
                        radius      :: Int
                     } deriving (Show,Eq)


main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    dodgers <- varCreate []
    varUpdate dodgers generateMoves 
    
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
    
        doPaint :: Var [[Dodger]] -> DC a -> Rect -> IO ()
        doPaint ds dc _ = do
            moves <- varGet ds
            mapM_ (\(Dodger _ position _ radius) -> do (circle dc position radius [])) (head moves)
            return ()
            
        updateDisplay :: Var [[Dodger]] -> Frame () -> IO ()
        updateDisplay ds f = do
            varUpdate ds (drop 1) 
            repaint f
            return ()
        
generateMoves :: [[Dodger]] -> [[Dodger]]
--generateMoves _ = restOfMoves [[(Dodger "1" (Point 0 50) (Point 5 0) 10)]] (mkStdGen 100)       
generateMoves _ = restOfMoves [] (mkStdGen 100)       

restOfMoves :: [[Dodger]] -> StdGen -> [[Dodger]]
restOfMoves [] gen = case new of 
                        Nothing -> restOfMoves [] gen'
                        Just d ->  [[d]] ++ restOfMoves [[d]] gen'
        where  (gen', new) = genRandDodger gen
        
restOfMoves xs gen = case new of 
                        Nothing -> [nextMoves] ++ restOfMoves [nextMoves] gen'
                        Just d ->  [d:nextMoves] ++ restOfMoves [d:nextMoves] gen'
        where   currentMoves = last xs
                nextMoves = filter inDisplay (map dodgerMove currentMoves)
                dodgerMove :: Dodger -> Dodger    
                dodgerMove (Dodger id (Point x y) speed@(Point vx vy) radius) =
                            (Dodger id (Point (x+vx) (y+vy)) speed radius)
                inDisplay :: Dodger -> Bool
                inDisplay (Dodger _ (Point x y) _ _) = (x < maxX) && (y < maxY)
                (gen', new) = genRandDodger gen
                    
genRandDodger :: StdGen -> (StdGen, Maybe Dodger)    
genRandDodger gen = if new == 1
                    then
                        if isX
                        then
                            (gen5, Just (Dodger "X" (Point 0 start) (Point speed 0) radius))
                        else
                            (gen5, Just (Dodger "Y" (Point start 0) (Point 0 speed) radius))
                    else
                        (gen1, Nothing)
                    where
                        (new, gen1)     = randomR (1,10)   gen  :: (Int, StdGen)
                        (radius, gen2)  = randomR (10,20)  gen1 :: (Int, StdGen)
                        (isX, gen3)     = random           gen2 :: (Bool, StdGen)
                        (start, gen4)   = randomR (1,maxX) gen3 :: (Int, StdGen)
                        (speed, gen5)   = randomR (1,20)    gen4 :: (Int, StdGen)
                    
                    
                            
                     