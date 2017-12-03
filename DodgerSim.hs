module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
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

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 

    bubbles <- varCreate []
    varUpdate bubbles createBubbles
    
    mouseDrag <- varCreate (Drag (Point 0 0) (Point 0 0))
   
    f <- frameFixed [] 
    
    set f [ text := "Bubble Sim", 
            bgcolor     := white, 
            layout      := space maxX maxY,
            on paint    := doPaint bubbles,
            on click    := onMouseLeftDown f mouseDrag,
            on unclick  := onMouseLeftUp f mouseDrag,
            on drag     := onMouseDrag f mouseDrag
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
            
        onMouseLeftDown :: Frame () -> Var Drag -> Point -> IO ()
        onMouseLeftDown f d p = do 
            varSet d (Drag p (Point 0 0))
            return ()

        onMouseLeftUp :: Frame () -> Var Drag -> Point -> IO ()
        onMouseLeftUp f d p = do
            (Drag p1 p2) <- varGet d
            withClientDC f (\dc -> drawBand dc p1 p2) -- undraw
            varSet d (Drag (Point 0 0) (Point 0 0))
            return ()
             
        onMouseDrag :: Frame () -> Var Drag -> Point -> IO ()
        onMouseDrag f d p = do 
            (Drag p1 p2) <- varGet d
            varSet d (Drag p1 p)
            withClientDC f (\dc -> drawBand dc p1 p2) -- undraw old
            withClientDC f (\dc -> drawBand dc p1 p) -- draw new
            return ()
             
        onClickLeftPaint :: DC a -> Point -> IO ()
        onClickLeftPaint dc p = do 
            circle dc p 20 []
            return ()

        drawBand :: DC a -> Point -> Point -> IO ()
        drawBand _ (Point 0 0) _ = return ()
        drawBand _ _ (Point 0 0) = return ()
        drawBand dc p1 p2 = do 
            dcSetLogicalFunction dc wxINVERT
            drawRect dc (rectBetween p1 p2) [penWidth := 1]
            return ()
  
 
        
        
createBubbles :: [[Bubble]] -> [[Bubble]]
createBubbles _ = moveBubbles [] 100 (mkStdGen 0)
    
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
updateBubble b bs = case mb of
                Nothing -> b
                Just b' -> b'
    where mb = find (\b' ->  not (bubbleOverlapsOthers b' others)) (candidateMovesOrdered b bs)
          others = filter (not . isSameBubble b) bs
             
-- returns true if bubbles have the same id    
isSameBubble :: Bubble -> Bubble -> Bool
isSameBubble (Bubble bid1 _ _ _) (Bubble bid2 _ _ _) = bid1 == bid2
   
-- checks if bubble overlaps any in a list of bubbles
bubbleOverlapsOthers :: Bubble -> [Bubble] -> Bool
bubbleOverlapsOthers b = foldl (\acc b' -> acc || (bubblesOverlap b b')) False
   
-- checks if two bubbles overlap  (i.e. square bubbles) 
bubblesOverlap :: Bubble -> Bubble -> Bool
bubblesOverlap (Bubble _ (Point x1 y1) _ r1) (Bubble _ (Point x2 y2) _ r2) = 
    regionsOverlap (Point (x1-r1) (y1-r1)) (Point (x1+r1) (y1+r1)) (Point (x2-r2) (y2-r2)) (Point (x2+r2) (y2+r2))
   
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

distanceBubbles :: Bubble -> Bubble -> Int
distanceBubbles (Bubble _ (Point x1 y1) _ r1) (Bubble _ (Point x2 y2) _ r2) =
    round $ sqrt(fromIntegral((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))) - fromIntegral (r1 + r2)
 
distanceFromBubbles :: Bubble -> [Bubble] -> Int
distanceFromBubbles b bs = foldl (\acc b' -> acc + (distanceBubbles b b')) 0 bs

-- returns a list of bubbles that interset a region            
findBubblesInRegion :: [Bubble] -> Point -> Point -> [Bubble]
findBubblesInRegion bs p1 p2 =
    foldl (\acc b@(Bubble _ (Point xb yb) _ rb) -> if (regionsOverlap p1 p2 (Point (xb-rb) (xb+rb)) (Point (yb-rb) (yb+rb))) then b:acc else acc) [] bs
            
regionsOverlap :: Point -> Point -> Point -> Point -> Bool
regionsOverlap (Point x11 y11) (Point x12 y12) (Point x21 y21) (Point x22 y22) =     
    x22 >= x11 && x12 >= x21 && y22 >= y11 && y12 >= y21
    

candidateMoves :: Bubble -> [Bubble]
candidateMoves (Bubble bid (Point x y) (Point vx vy) r) = 
    if vx > 0 
    then
        map (\(Point dx dy) -> (Bubble bid (Point (x+dx) (y+dy)) (Point vx vy) r)) [(Point d 0), (Point 0 (-d)), (Point 0 d), (Point d (-d)), (Point d d)]
    else
        map (\(Point dx dy) -> (Bubble bid (Point (x+dx) (y+dy)) (Point vx vy) r)) [(Point 0 d), (Point (-d) 0), (Point d 0), (Point (-d) d), (Point d d)]
    where d = vx+vy
 
-- Bubble to find near by bubbles for -> all bubbles -> near by bubbles
nearByBubbles :: Bubble -> [Bubble] -> [Bubble]
nearByBubbles (Bubble _ (Point x y) (Point vx vy) r) bs
    = findBubblesInRegion bs (Point (x-d) (y-d)) (Point (x+d) (y+d))
        where d = vx + vy + r
               
candidateMovesOrdered :: Bubble -> [Bubble] -> [Bubble]
candidateMovesOrdered b bs = 
    case nearBy of
        [] ->  possibleMoves
        otherwise -> sortBy (\b1 b2 -> (compare (distanceFromBubbles b2 nearBy) (distanceFromBubbles b1 nearBy))) possibleMoves
        where   nearBy = nearByBubbles b (filter (not . isSameBubble b) bs)
                possibleMoves = candidateMoves b
    
      