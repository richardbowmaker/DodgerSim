module Main where
import Graphics.UI.WX
import System.Random
import Data.List
import Debug.Trace
import System.IO
import Data.Char

-- size of display
maxX, maxY :: Int
maxY = 600
maxX = 600

data Bubble = Bubble {  bid         :: Int,
                        position    :: Point,
                        speed       :: Point,
                        radius      :: Int
                     } deriving (Eq, Show)

 
b1 = (Bubble 1 (Point 0 200) (Point 10 0) 10)                     
b2 = (Bubble 2 (Point 200 0) (Point 0 10) 10)  
b3 = (Bubble 3 (Point 300 0) (Point 0 10) 10)  
bs = [b1,b2]                   
                   
    
bs2 =  [(Bubble 4 (Point 20 300) (Point 10 0) 11),
        (Bubble 5 (Point 40 360) (Point 20 0) 16),
        (Bubble 6 (Point 81 327) (Point 9 0) 10),
        (Bubble 7 (Point 328 392) (Point 0 14) 13),
        (Bubble 8 (Point 58 300) (Point 1 0) 17),
        (Bubble 9 (Point 1470 330) (Point 15 0) 16),
        (Bubble 10 (Point 565 300) (Point 5 0) 12),
        (Bubble 11 (Point 300 342) (Point 0 3) 13),
        (Bubble 12 (Point 720 300) (Point 6 0) 14),
        (Bubble 13 (Point 2268 300) (Point 18 0) 13)]
 
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

    --bs <- varGet bubbles
    -- mapM_ writeLogFile (take 10 bs)
    
    --putStr (bubblesListToString (take 10 bs))
 
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

            where   currentBubbles = case xs of
                        [] -> []
                        xs -> last xs
                    
                    -- update all bubble postitions and discard any that have moved outside
                    -- the display area 
                    updatedBubbles = filter inDisplay (updateBubbles currentBubbles)
                    
                    -- is the bubble still in the display area
                    inDisplay :: Bubble -> Bool
                    inDisplay (Bubble _ (Point x y) _ _) = (x < maxX) && (y < maxY)
                    
                    -- generate new bubble
                    (gen', nid, mb) = genRandBubble gen nid (length updatedBubbles)
                    
                    -- add to list of bubbles provided it does not overlap
                    moves = case mb of
                        Nothing -> updatedBubbles
                        Just b  -> case bubbleOverlapsOthers b updatedBubbles of
                                True -> updatedBubbles
                                False -> b:updatedBubbles
                                                                      
-- generate a random doger (10%) or nothing (90%)
genRandBubble :: StdGen -> Int -> Int -> (StdGen, Int, Maybe Bubble)    
genRandBubble gen nid size = if new == 1 && size < 10
                    then
                        if isX
                        then
                            (gen5, (nid+1), Just (Bubble nid (Point 0 start) (Point speed 0) radius))
                        else
                            (gen5, (nid+1), Just (Bubble nid (Point start 0) (Point 0 speed) radius))
                    else
                        (gen1, nid, Nothing)
                    where
                        (new, gen1)     = randomR (1,10)   gen  :: (Int, StdGen)
                        (radius, gen2)  = randomR (10,20)  gen1 :: (Int, StdGen)
                        (isX, gen3)     = random           gen2 :: (Bool, StdGen)
                        (start, gen4)   = randomR (1,maxX) gen3 :: (Int, StdGen)
                        (speed, gen5)   = randomR (1,20)   gen4 :: (Int, StdGen)

                                                              
     
                  

-- selects each bubble in turn and calculates its next move
updateBubbles :: [Bubble] -> [Bubble]
updateBubbles bs = map (\b -> updateBubble b bs) bs

-- updates a bubble, updated bubble must not overlap other bubbles
updateBubble :: Bubble -> [Bubble] -> Bubble
updateBubble b bs = case validMove of
                        Just b' -> b'
                        Nothing -> b  -- no valid move possible
    where validMove = find (\b' -> not (bubbleOverlapsOthers b' others)) (possibleMoves b) 
          others = filter (not . isSameBubble b) bs
  
-- returns a list of candidate moves that a bubble can make in priority order 
possibleMoves :: Bubble -> [Bubble]
possibleMoves b@(Bubble id (Point x y) speed@(Point vx vy) radius) =
                [ (Bubble id (Point (x+vx) (y+vy)) speed radius),
                  (Bubble id (Point (x+vy) (y+vx)) speed radius),
                  (Bubble id (Point (x-vy) (y-vx)) speed radius) ]
                  
-- returns true of bubbles have the same id    
isSameBubble :: Bubble -> Bubble -> Bool
isSameBubble (Bubble bid1 _ _ _) (Bubble bid2 _ _ _) = bid1 == bid2
   
-- checks if bubble overlaps any in a list of bubbles
bubbleOverlapsOthers :: Bubble -> [Bubble] -> Bool
bubbleOverlapsOthers b = foldl (\acc b' -> acc || (bubblesOverlap b b')) False
   
-- checks if two bubbles overlap   
bubblesOverlap :: Bubble -> Bubble -> Bool
bubblesOverlap (Bubble _ (Point x1 y1) _ r1) (Bubble _ (Point x2 y2) _ r2) = 
    (x2+r2) > (x1-r1) && (x1+r1) > (x2-r2) && (y2+r2) > (y1-r1) && (y1+r1) > (y2-r2)
   

-----------------------------------------------  
-----------------------------------------------  
    
writeLogFile :: [Bubble] -> IO ()
writeLogFile bs = do
    handle <- openFile "D:\\_Rick's\\haskell\\DodgerSim\\log.txt" AppendMode
    hPutStr handle "-----------------------------------------------------------\n"
    mapM_ (\b ->  do hPutStr handle (show b ++ "\n")) bs
    hClose handle
    return ()
 
intToString :: Int -> String
intToString 0 = "0"
intToString n = intToString(n `quot` 10) ++ [chr(48 + n `mod` 10)]
 
bubblesListToString :: [[Bubble]] -> String
bubblesListToString lbs = "[" ++ (foldl (\acc bs -> acc ++ bubblesToString bs ++ ",") "" lbs ) ++ "]"

bubblesToString :: [Bubble] -> String
bubblesToString bs = "[" ++ (foldl (\acc b -> acc ++ bubbleToString b ++ ",") "" bs) ++ "]"
 
bubbleToString :: Bubble -> String 
bubbleToString (Bubble bid (Point x y) speed@(Point vx vy) radius) = "bid = " ++ intToString bid
 
--instance Show Bubble where  
--    show (Bubble bid (Point x y) speed@(Point vx vy) radius) = "bid = " ++ (intToString bid) ++ "\n"
                           
  