module Main where
import Graphics.UI.WX
import System.Random
import Data.List

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

-- generates an list of lists comprising all the dodgers and their moves as an infinite list.
-- the outer list is a time array, t0, t1, t2 etc with each element being a list of dodgers and there current position at
-- time tn. 
-- The dodgers are are added to the timeline randomly and will move either vertically or horizontally



restOfMoves :: [[Dodger]] -> StdGen -> [[Dodger]]
restOfMoves xs gen = [moves] ++ restOfMoves [moves] gen'

            where   currentMoves = case xs of
                        [] -> []
                        xs -> last xs
                    
                    -- update all dodger postitions and discard any that have moved outside
                    -- the display area 
                    -- updatedMoves = filter inDisplay (map dodgerMove currentMoves)
                    updatedMoves = findValidMoves (allMoves currentMoves)
                 
                    -- update the position of the dodger
                    dodgerMove :: Dodger -> Dodger    
                    dodgerMove (Dodger id (Point x y) speed@(Point vx vy) radius) =
                                (Dodger id (Point (x+vx) (y+vy)) speed radius)
                     
                    -- is the dodger still in the display area
                    inDisplay :: Dodger -> Bool
                    inDisplay (Dodger _ (Point x y) _ _) = (x < maxX) && (y < maxY)
                    
                    -- generate new dodger and insert into list of dodgers 
                    (gen', md) = genRandDodger gen
                    moves = case md of
                        Nothing -> updatedMoves
                        Just d  -> d:updatedMoves
                       
                    -- generate a random doger (10%) or nothing (90%)
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
                                            (speed, gen5)   = randomR (1,20)   gen4 :: (Int, StdGen)
                    
                                                              
-- takes the list of bubbles and returns a list of lists representing the candidate set
-- of every combination of possible bubble moves. 
-- One of those will be chosen to be the next set of actual moves, i.e. no two bubbles overlap
-- i.e each bubble can move in each its preferred direction x or y, at right angles to that or not at all
allMoves :: [Dodger] -> [[Dodger]]
allMoves [] = [[]]
allMoves (d@(Dodger id (Point x y) speed@(Point vx vy) radius):ds) 
    = (:) <$> [ (Dodger id (Point (x+vx) (y+vy)) speed radius),
                (Dodger id (Point (x+vy) (y+vx)) speed radius),
                (Dodger id (Point (x-vy) (y-vx)) speed radius),
                d]
          <*> allMoves ds

-- search the list of all possible bubble moves to find the first one where no two
-- bubbles overlap         
findValidMoves :: [[Dodger]] -> [Dodger]
findValidMoves ds = case find noOverlaps ds of
                        Just ds' -> ds'
                        Nothing  -> last ds
  
-- checks that a list of bubbles has no overlaps  
noOverlaps :: [Dodger] -> Bool
noOverlaps ds = foldl (\result (d1,d2) -> result && not (overlaps d1 d2)) True (allPossiblePairs ds)
   
-- checks if two bubbles overlap   
overlaps :: Dodger -> Dodger -> Bool
overlaps (Dodger _ (Point x1 y1) _ r1) (Dodger _ (Point x2 y2) _ r2) = (x2+r2) < (x1-r1) || (x1+r1) < (x2-r2)

-- takes a list of bubbles and returns all possible pairings, i.e. 2Cn, so that
-- the list of bubbles can be checked for no overlaps between any of the bubbles
allPossiblePairs :: [Dodger] -> [(Dodger,Dodger)]
allPossiblePairs [] = []
allPossiblePairs (x:xs) = [(x,b) | b <- xs] ++ (allPossiblePairs xs)




                     