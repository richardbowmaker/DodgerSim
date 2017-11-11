module Main where
import Graphics.UI.WX
import System.Random
import Data.List
import Debug.Trace
import System.IO

-- size of display
maxX, maxY :: Int
maxY = 600
maxX = 600

data Bubble = Bubble {  id          :: String,
                        position    :: Point,
                        speed       :: Point,
                        radius      :: Int
                     } deriving (Show,Eq)


b1 = (Bubble "1" (Point 0 200) (Point 10 0) 10)                     
b2 = (Bubble "2" (Point 200 0) (Point 0 10) 10)  
b3 = (Bubble "3" (Point 300 0) (Point 0 10) 10)  
bs = [b1,b2]                   
     
bs2 =  [(Bubble "X" (Point 20 300) (Point 10 0) 11),
    (Bubble "X" (Point 40 360) (Point 20 0) 16),
    (Bubble "X" (Point 81 327) (Point 9 0) 10),
    (Bubble "Y" (Point 328 392) (Point 0 14) 13),
    (Bubble "X" (Point 58 300) (Point 1 0) 17),
    (Bubble "X" (Point 1470 330) (Point 15 0) 16),
    (Bubble "X" (Point 565 300) (Point 5 0) 12),
    (Bubble "Y" (Point 300 342) (Point 0 3) 13),
    (Bubble "X" (Point 720 300) (Point 6 0) 14),
    (Bubble "X" (Point 2268 300) (Point 18 0) 13)]



   
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    bubbles <- varCreate []
    varUpdate bubbles generateMoves
   
    f <- frameFixed [] 
    
    set f [ text := "Bubble Sim", 
            bgcolor := white, 
            layout := space maxX maxY,
            on paint := doPaint bubbles
          ]
      
    -- create a timer that updates the display
    t <- timer f [interval := 100, on command := updateDisplay bubbles f]

 --   bs <- varGet bubbles
 --   mapM_ writeLogFile (take 3 bs)
 
    return () 
    
    where
    
        doPaint :: Var [[Bubble]] -> DC a -> Rect -> IO ()
        doPaint ds dc _ = do
            moves <- varGet ds
            mapM_ (\(Bubble _ position _ radius) -> do (circle dc position radius [])) (head moves)
            return ()
            
        updateDisplay :: Var [[Bubble]] -> Frame () -> IO ()
        updateDisplay ds f = do
            varUpdate ds (drop 1) 
            repaint f
            return ()
        
generateMoves :: [[Bubble]] -> [[Bubble]]
generateMoves _ = restOfMoves [] 0 (mkStdGen 500)
--generateMoves _ = [bs2] ++ restOfMoves [bs2] 0 (mkStdGen 200)
     
-- generates an list of lists comprising all the bubbles and their moves as an infinite list.
-- the outer list is a time array, t0, t1, t2 etc with each element being a list of bubbles and there current position at
-- time tn. 
-- The bubbles are are added to the timeline randomly and will move either vertically or horizontally

restOfMoves :: [[Bubble]] -> Int -> StdGen -> [[Bubble]]
restOfMoves xs nid gen = [moves] ++ (restOfMoves [moves] nid gen')

            where   currentMoves = case xs of
                        [] -> []
                        xs -> last xs
                    
                    -- update all bubble postitions and discard any that have moved outside
                    -- the display area 
                    updatedMoves = filter inDisplay (findValidMoves (allMoves currentMoves))
                    
                    -- is the bubble still in the display area
                    inDisplay :: Bubble -> Bool
                    inDisplay (Bubble _ (Point x y) _ _) = (x < maxX) && (y < maxY)
                    
                    -- generate new bubble
                    (gen', nid, mb) = genRandBubble gen nid (length updatedMoves)
                    
                    -- add to list of bubbles provided it does not overlap
                    moves = case mb of
                        Nothing -> updatedMoves
                        Just b  -> case bubbleOverlaps b updatedMoves of
                                True -> updatedMoves
                                False -> b:updatedMoves
                                                                      
-- generate a random doger (10%) or nothing (90%)
genRandBubble :: StdGen -> Int -> Int -> (StdGen, Int, Maybe Bubble)    
genRandBubble gen nid size = if new == 1 && size < 10
                    then
                        if isX
                        then
                            (gen5, (nid+1), Just (Bubble ("X") (Point 0 start) (Point speed 0) radius))
                        else
                            (gen5, (nid+1), Just (Bubble ("Y") (Point start 0) (Point 0 speed) radius))
                    else
                        (gen1, nid, Nothing)
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
allMoves :: [Bubble] -> [[Bubble]]
allMoves [] = [[]]
allMoves (d@(Bubble id (Point x y) speed@(Point vx vy) radius):ds) 
    = (:) <$> [ (Bubble id (Point (x+vx) (y+vy)) speed radius),
--                (Bubble id (Point (x+vy) (y+vx)) speed radius),
--                (Bubble id (Point (x-vy) (y-vx)) speed radius),
                d]
          <*> allMoves ds

-- search the list of all possible bubble moves to find the first one where no two
-- bubbles overlap         
findValidMoves :: [[Bubble]] -> [Bubble]
findValidMoves ds = case find noOverlaps ds of
                        Just ds' -> ds'
                        Nothing  -> last ds
  
-- checks that a list of bubbles has no overlaps  
noOverlaps :: [Bubble] -> Bool
noOverlaps ds = foldl (\result (d1,d2) -> result && not (overlaps d1 d2)) True (allPossiblePairs ds)


bubbleOverlaps :: Bubble -> [Bubble] -> Bool
bubbleOverlaps b = foldl (\acc b' -> acc || (overlaps b b')) False
   
-- checks if two bubbles overlap   
overlaps :: Bubble -> Bubble -> Bool
overlaps (Bubble _ (Point x1 y1) _ r1) (Bubble _ (Point x2 y2) _ r2) = 
    (x2+r2) > (x1-r1) && (x1+r1) > (x2-r2) && (y2+r2) > (y1-r1) && (y1+r1) > (y2-r2)

-- takes a list of bubbles and returns all possible pairings, i.e. 2Cn, so that
-- the list of bubbles can be checked for no overlaps between any of the bubbles
allPossiblePairs :: [Bubble] -> [(Bubble,Bubble)]
allPossiblePairs [] = []
allPossiblePairs (x:xs) = [(x,b) | b <- xs] ++ (allPossiblePairs xs)



writeLogFile :: [Bubble] -> IO ()
writeLogFile bs = do
    handle <- openFile "D:\\_Rick's\\haskell\\DodgerSim\\log.txt" AppendMode
    hPutStr handle "-----------------------------------------------------------\n"
    mapM_ (\b ->  do hPutStr handle (show b ++ "\n")) bs
    hClose handle
    return ()
    


                     