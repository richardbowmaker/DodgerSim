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
                     } deriving (Eq)

   
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


nextBubbleMove :: Bubble -> [Bubble] -> Maybe Bubble
nextBubbleMove b bs = find (\b' -> isValidMove b' bs) (possibleMoves b)

isValidMove :: Bubble -> [Bubble] -> Bool
isValidMove b@(Bubble bid _ _ _) = foldl (\acc b'@(Bubble bid' _ _ _) -> if bid == bid' then acc && True else acc && (not (overlaps b b'))) True

possibleMoves :: Bubble -> [Bubble]
possibleMoves b@(Bubble id (Point x y) speed@(Point vx vy) radius) =
                [ (Bubble id (Point (x+vx) (y+vy)) speed radius),
                  (Bubble id (Point (x+vy) (y+vx)) speed radius),
                  (Bubble id (Point (x-vy) (y-vx)) speed radius),
                  b]
                  
                  
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
 
instance Show Bubble where  
    show (Bubble bid (Point x y) speed@(Point vx vy) radius) = "bid = " ++ (intToString bid) ++ "\n"
    
  
findReplace :: (Eq a) => (a -> Bool) -> a -> [a] -> [a]
findReplace _ _ [] = []
findReplace f x' (x:xs) = (if f x then x' else x):(findReplace f x' xs)

isThree :: Int -> Bool
isThree n = n == 3

  