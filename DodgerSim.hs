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


main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    bubbles <- varCreate []
    varUpdate bubbles createBubbles
   
    f <- frameFixed [] 
    
    set f [ text := "Bubble Sim", 
            bgcolor := white, 
            layout := space maxX maxY,
            on paint := doPaint bubbles
          ]
      
    -- create a timer that updates the display
    t <- timer f [interval := 50, on command := updateDisplay bubbles f]

    return () 
    
    where
    
        doPaint :: Var [[Bubble]] -> DC a -> Rect -> IO ()
        doPaint ds dc _ = do
            bubbles <- varGet ds
            mapM_ (\(Bubble _ position _ radius) -> do (circle dc position radius [])) (head bubbles)
            return ()
            
        updateDisplay :: Var [[Bubble]] -> Frame () -> IO ()
        updateDisplay ds f = do
            varUpdate ds (drop 1) 
            repaint f
            return ()
        
createBubbles :: [[Bubble]] -> [[Bubble]]
createBubbles _ = moveBubbles [] 200 (mkStdGen 0)
    
-- generates an list of lists comprising all the bubbles and their moves as an infinite list.
-- the outer list is a time array, t0, t1, t2 etc with each element being a list of bubbles and there current position at
-- time tn. 
-- The bubbles are are added to the timeline randomly and will move either vertically or horizontally
moveBubbles :: [[Bubble]] -> Int -> StdGen -> [[Bubble]]
moveBubbles bs nid gen = [bubbles] ++ (moveBubbles [bubbles] nid' gen')

    where   currentBubbles = lastList bs
          
            -- update all bubble postitions and discard any that have moved outside
            -- the display area 
            updatedBubbles = filter inDisplay (updateBubbles currentBubbles)
                                             
            -- generate new random bubble
            (gen', nid', mb) = genRandBubble gen nid
            
            -- add new bubble to list
            bubbles = addBubble mb updatedBubbles
                                                                                                                                           
-- generate a random bubble (10%) or nothing (90%)
genRandBubble :: StdGen -> Int -> (StdGen, Int, Maybe Bubble)    
genRandBubble gen nid = 
    if new == 1
    then
        if isX
        then
            (gen5, bid, Just (Bubble bid (Point 0 start) (Point speed 0) radius))
        else
            (gen5, bid, Just (Bubble bid (Point start 0) (Point 0 speed) radius))
    else
        (gen1, bid, Nothing)
    where
        (new, gen1)     = randomR (1,10)     gen  :: (Int, StdGen)
        (isX, gen2)     = random             gen1 :: (Bool, StdGen)
        (start, gen3)   = randomR (250, 350) gen2 :: (Int, StdGen)
        (speed, gen4)   = randomR (1,10)     gen3 :: (Int, StdGen)
        (radius, gen5)  = randomR (10,20)    gen4 :: (Int, StdGen)
        bid = nid + 1
                               
-- selects each bubble in turn and calculates its next move
updateBubbles :: [Bubble] -> [Bubble]
updateBubbles bs = map (\b -> updateBubble b (filter (not . isSameBubble b) bs)) bs

-- updates a bubble, updated bubble must not overlap other bubbles
updateBubble :: Bubble -> [Bubble] -> Bubble
updateBubble b bs = case validMove of
                        Just b' -> b'
                        Nothing -> b  -- no valid move possible, stay put
                        
    where validMove = find (\b' -> not (bubbleOverlapsOthers b' bs)) (possibleMoves b) 

-- returns a list of candidate moves that a bubble can make in priority order 
possibleMoves :: Bubble -> [Bubble]
possibleMoves b@(Bubble id (Point x y) speed@(Point vx vy) radius) =
                [ (Bubble id (Point (x+vx) (y+vy)) speed radius),
                  (Bubble id (Point (x-vy) (y-vx)) speed radius) ]
--                  (Bubble id (Point (x+vy) (y+vx)) speed radius) ]
                  
-- returns true if bubbles have the same id    
isSameBubble :: Bubble -> Bubble -> Bool
isSameBubble (Bubble bid1 _ _ _) (Bubble bid2 _ _ _) = bid1 == bid2
   
-- checks if bubble overlaps any in a list of bubbles
bubbleOverlapsOthers :: Bubble -> [Bubble] -> Bool
bubbleOverlapsOthers b = foldl (\acc b' -> acc || (bubblesOverlap b b')) False
   
-- checks if two bubbles overlap   
bubblesOverlap :: Bubble -> Bubble -> Bool
bubblesOverlap (Bubble _ (Point x1 y1) _ r1) (Bubble _ (Point x2 y2) _ r2) = 
    (x2+r2) > (x1-r1) && (x1+r1) > (x2-r2) && (y2+r2) > (y1-r1) && (y1+r1) > (y2-r2)
   
-- returns last list of a list or [] if empty
lastList :: [[a]] -> [a]
lastList [] = []
lastList xs = last xs

-- add a bubble in a Maybe to start of list, provided it does not overlap other bubbles in the list
addBubble :: Maybe Bubble -> [Bubble] -> [Bubble]
addBubble Nothing bs = bs
addBubble (Just b) bs = if (length bs > 20) || (bubbleOverlapsOthers b others) then bs else b:bs
    where others = filter (not . isSameBubble b) bs

-- is the bubble still in the display area
inDisplay :: Bubble -> Bool
inDisplay (Bubble _ (Point x y) _ _) = (x < maxX) && (y < maxY)


