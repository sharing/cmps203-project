
module Funkytron where

import Array
import List
--import Monad
import Maybe
import SOE
--import Control.Monad.State
--import Graphics.HGL.Window

import Data.List.Split --to use, must run in command prompt window "cabal install split"
import System.Exit

-- import qualified GraphicsWindows as GW (getEvent)

printState' :: RobotState -> Window -> IO ()
printState' s w
 = if (exit s) == True 
	then do 
			closeWindow w
			exitSuccess
	else
		do 
		putStrLn "Current Robot State:"
		putStrLn ("  Position:  " ++ show (position s))
		putStrLn ("  Facing:    " ++ show (facing s))
		putStrLn ("  Coins:     " ++ show (pocket s))
		putStrLn ("  Collision: " ++ show (collided s))
		putStrLn ("  Programs:  " ++ show (programs s))
		putStrLn ("  Turns:  " ++ show (turns s))
		putStrLn ("  Moves:  " ++ show (moves s))
		putStrLn ("  Fuel:  " ++ show (fuel s))
		putStrLn ""


	
data RobotState 
   = RobotState 
        { position  :: Position
        , facing    :: Direction
        , pen       :: Bool 
        , color     :: Color
        , treasure  :: [Position]
        , enemies   :: [Position]
        , pocket    :: Int
        , turns     :: Int
        , moves     :: Int
        , fuel      :: Int
        , exit	    :: Bool
        , collided  :: Bool
        , zapped    :: Bool
        , laserTo   :: Position
        , programs  :: ProgramStore
        , grid :: Grid
        }
     deriving Show

s0 :: RobotState
s0 = RobotState { position = (0,0)
                , pen      = False
                , color    = Blue
                , facing   = North
                , treasure = tr
                , enemies  = [(-4,-1),(5,3),(1,9)]
                , pocket   = 0
                , turns    = 0
                , moves    = 0
                , fuel     = 100
                , exit	    = False
                , collided  = False
                , zapped    = False
                , laserTo   = (0,0)
                , programs  = programStorage
                , grid      = g2
                }
					
tr :: [Position]
tr = [(x,y) | x <- [-13,-11 .. 1], y <- [9,11 .. 15]]

programStorage :: ProgramStore
programStorage = []

--type ProgramStore = [(String,[Command])]

type ProgramStore = [(String,[String])]

--initEnemies :: [Position]
--initEnemies = [(x,y) | x <- [-13,-11 .. -1], y <- [4,8 .. 16]]

g0 :: Grid
g0 = array ((-size,-size),(size,size))
       ([ ((i, size),nb)   | i <- r ] ++
        [ ((i,-size),sb)   | i <- r ] ++
        [ (( size,i),eb)   | i <- r ] ++
        [ ((-size,i),wb)   | i <- r ] ++
        [ ((i,j),interior) | i <- r, j <- r ] ++
        [ ((size,size), nec),((size,-size), sec),
          ((-size,size),nwc),((-size,-size),swc)] )
     where r = [1-size .. size-1]

 
drawGrid :: Window -> RobotState -> IO ()
drawGrid w wld
  = let (low@(xMin,yMin),hi@(xMax,yMax)) = bounds (grid wld)
        (x1,y1) = trans low
        (x2,y2) = trans hi
    in do 
		drawLine w wc (x1-d,y1+d) (x1-d,y2-d)
		drawLine w wc (x1-d,y1+d) (x2+d,y1+d)
		sequence_ [drawPos w (trans (x,y)) ((grid wld) `at` (x,y)) 
			| x <- [xMin..xMax], y <- [yMin..yMax]]


spaceClose :: Window -> IO ()
spaceClose w
  = do k <- getKey w
       if k==' ' || k == '\x0'
          then putStr "> "
          else spaceClose w

spaceWait :: Window -> IO ()
spaceWait w
  = do k <- getLine
       if k== " " then return ()
                 else spaceWait w

-- //////////////////////////////////////////////////////////////

data Command = Lt
             | Rt
             | Fd Int
             | Bk Int
             | Exit
             | No_Op
             | Pick
             | ZapCmd
             | Call String -- call program (string:name)
             | Define (String,[String]) -- define program (string:name)
          deriving Show
					
main' = do 
		w <- openWindowEx "Robot World" (Just (0,0))
			(Just (xWin,yWin)) drawBufferedGraphic --1000
		sim s0 w

sim :: RobotState -> Window -> IO ()
sim s w = runGraphics $ 
			do
				drawGrid w s
				drawCoins w s
				drawEnemies w s
				drawPlayer w s
				printState' s w
				maybeGetWindowEvent w
				sim' s w

sim' :: RobotState -> Window -> IO ()
sim' s w = runGraphics $ 
			do
				maybeGetWindowEvent w
				putStr "> "
				cmdStr <- getLine
				let cmd = parse (splitOn " " cmdStr)
				let s' = run_c cmd (s{zapped=False})
				let s'' = moveEnemies s'
				let s''' = checkCollision s''
				printState' s''' w
				-- erase
				eraseCoins w s
				eraseEnemies w s
				erasePlayer w s
				drawLaser w s Black -- black is the new erase
				-- draw
				drawGrid w s'''
				drawCoins w s'''
				drawEnemies w s'''
				drawLaser w s''' Cyan
				drawPlayer w s'''
				sim' s''' w

spaceDo :: String -> Window -> IO ()
spaceDo k w
  = --do k <- getLine
       case k of
			_ -> return ()
			--" " -> return ()
                 --else spaceWait w

run_c :: [Command] -> RobotState -> RobotState
run_c (Call name:[]) s = run_c (parse (getProgramCmdsByName name s)) s
run_c ((Define (name, strings)):xs) s = (s{programs = (programs s)++[(name,strings)]})
run_c (Call name:xs) s = run_c (parse (getProgramCmdsByName name s)) (run_c xs s)
run_c (Fd x:[]) s = do  (s {position = newPos})
                    where newPos = (movePos (position s) x (facing s) (grid s))
run_c (Bk x:[]) s = do  (s {position = newPos})
                    where newPos = (movePos (position s) (negate x) (facing s) (grid s))
run_c (Lt:[]) s = do (s {facing = left (facing s)})
run_c (Rt:[]) s = do (s {facing = right (facing s)})
run_c (Exit:[]) s = do (s {exit = True})
run_c (No_Op:[]) s = s
run_c (Pick:[]) s = pickCoin s
run_c (ZapCmd:[]) s = zapCmd s
run_c (x:xs) s = run_c [x] (run_c xs s)

getProgramCmdsByName :: String -> RobotState -> [String]
getProgramCmdsByName name s = do 
								p <- mapMaybe (`lookup` (programs s)) [name] 
								if (p == [])
								then return []
								else return p !! 0
--snd (fromMaybe (Just []) (find (\(n,c)->Bool=name == fst) (programs s)) [])

parse :: [String] -> [Command]
parse [] = [No_Op]
parse (cmd:[]) = case cmd of
			"left" -> [Lt]
			"right" -> [Rt]
			"exit" -> [Exit]
			"pick" -> [Pick]
			"zap" -> [ZapCmd]
			_ -> if (elem '_' cmd)
				then parse (splitOn "_" cmd)
				else [No_Op]
parse ("define":val:[]) = [Define (val,["null"])]
parse ("define":val:xs) = [Define (val,xs)]
parse (cmd:val:[]) = case cmd of
			"forward" -> [Fd (read $ val :: Int)]
			"backward" -> [Bk (read $ val :: Int)]
			"call" -> [Call val]
			_ -> parse [cmd] ++ parse [val]
parse (x:xs) = parse [x] ++ parse xs

movePos :: Position -> Int -> Direction -> Grid -> Position
movePos (x,y) v d g | (blocked (x,y) d g) == True = (x,y)
                    | otherwise = case d of
                            North ->  (x,y+v) 
                            South ->  (x,y-v) 
                            East  ->  (x+v,y) 
                            West  ->  (x-v,y) 


-- ///////////////////////////////////////////////////////////////////////////

type Position = (Int,Int)

data Direction = North | East | South | West
     deriving (Eq,Show,Enum)

right,left :: Direction -> Direction

right d = toEnum (succ (fromEnum d) `mod` 4)
left  d = toEnum (pred (fromEnum d) `mod` 4)


blocked   :: Position->Direction->Grid -> Bool
blocked pos face grid = face `notElem` (grid `at` pos)

--move = cond1 (isnt blocked)

--data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White

colors :: Array Int Color
colors = array (0,7) (zip [0..7] [Black .. White])

type Grid = Array Position [Direction]

at :: Grid -> Position -> [Direction]
at = (!)

size :: Int
size = 20

interior = [North, South, East, West]

nb  = [South, East, West]
sb  = [North, East, West]
eb  = [North, South, West]
wb  = [North, South, East]
nwc = [South, East]
nec = [South, West]
swc = [North, East]
sec = [North, West]


mkHorWall, mkVerWall :: Int -> Int -> Int -> [(Position,[Direction])]

mkHorWall x1 x2 y
  = [ ((x,y),  nb) | x <- [x1..x2] ] ++
    [ ((x,y+1),sb) | x <- [x1..x2] ]

mkVerWall y1 y2 x
  = [ ((x,y),  eb) | y <- [y1..y2] ] ++
    [ ((x+1,y),wb) | y <- [y1..y2] ]

g1 :: Grid
g1 = g0 // mkHorWall (-5) 5 10

mkBox :: Position -> Position -> [(Position,[Direction])]
mkBox (x1,y1) (x2,y2)
  = mkHorWall (x1+1) x2 y1 ++
    mkHorWall (x1+1) x2 y2 ++
    mkVerWall (y1+1) y2 x1 ++
    mkVerWall (y1+1) y2 x2

-- (//) :: Ix a => Array a b -> [(a,b)] -> Array a b
-- | colors // [(0,White) (7,Black)]

-- accum :: (Ix a) => (b->c->b) -> Array a b -> [(a,c)] -> Array a b

-- | [South, East, West] `intersect` [North, South, West]
-- | ===> [South, West]

g2 :: Grid
g2 = accum intersect g0 (mkBox (-15,8) (2,17))

g3 :: Grid
g3 = accum union g2 [((-7,17),interior),((-7,18),interior)]

drawLine :: Window -> Color -> Point -> Point -> IO ()
drawLine w c p1 p2
  = drawInWindowNow w (withColor c (line p1 p2))

drawCircle :: Window -> Color -> Point -> Int -> IO ()
drawCircle w c center radius
           = let (x,y) = center
             in 
                --putStrLn ("x:"++(show x)++" y:"++(show y)++" r:"++(show radius))
                drawInWindowNow w (withColor c (ellipse (x-radius,y-radius) (x+radius,y+radius)))

d :: Int
d = 5       -- half the distance between grid points

wc, cc :: Color
wc = Green  -- color of walls
cc = Yellow -- color of coins

xWin, yWin :: Int
xWin = 600
yWin = 500

pause n =
  do t0 <- timeGetTime
     let loop = do t1 <- timeGetTime
                   if (t1-t0) < n
                      then loop
                      else return ()
     loop

-------- sprite drawing/erase /////////////////////////////////////////////

drawPlayer :: Window -> RobotState -> IO ()
drawPlayer win state
           =   let (x,y) = trans (position state)
                   
               --in putStrLn ("x:"++(show x)++" y:"++(show y))
               in do drawCircle win Magenta (x,y) 10

erasePlayer :: Window -> RobotState -> IO ()
erasePlayer win state
           =   let (x,y) = trans (position state)
               --in putStrLn ("x:"++(show x)++" y:"++(show y))
               in do drawCircle win Black (x,y) 10

drawLaser :: Window -> RobotState -> Color -> IO ()
drawLaser win state color
           | (zapped state) == True = let (x,y) = trans (position state)
                                          (toX,toY) = trans (laserTo state)
                                      in do drawLine win color (x,y) (toX,toY)
           | True = return ()



drawPos :: Window -> Point -> [Direction] -> IO ()
drawPos w (x,y) ds
  = do if North `notElem` ds
          then drawLine w wc (x-d,y-d) (x+d,y-d)
          else return ()
       if East `notElem` ds
          then drawLine w wc (x+d,y-d) (x+d,y+d)
          else return ()

drawCoins :: Window -> RobotState -> IO ()
drawCoins w s = mapM_ (drawCoin w) (treasure s)

drawCoin :: Window -> Position -> IO ()
drawCoin w p = let (x,y) = trans p
               in do drawCircle w Yellow (x,y) 6

eraseCoins :: Window -> RobotState -> IO ()
eraseCoins w s = mapM_ (drawCoin w) (treasure s)

eraseCoin :: Window -> Position -> IO ()
eraseCoin w p = let (x,y) = trans p
               in do drawCircle w Black (x,y) 6

drawEnemies :: Window -> RobotState -> IO ()
drawEnemies w s = mapM_ (drawEnemy w) (enemies s)

drawEnemy :: Window -> Position -> IO ()
drawEnemy w p = let (x,y) = trans p
               in do drawCircle w Red (x,y) 9

eraseEnemies :: Window -> RobotState -> IO ()
eraseEnemies w s = mapM_ (eraseEnemy w) (enemies s)

eraseEnemy :: Window -> Position -> IO ()
eraseEnemy w p = let (x,y) = trans p
               in do drawCircle w Black (x,y) 9

trans :: Position -> Point
trans (x,y) = (div xWin 2 + 2*d*x, div yWin 2 - 2*d*y)

onCoin :: RobotState -> Bool
onCoin s = (position s) `elem` (treasure s)

moveEnemies :: RobotState -> RobotState
moveEnemies s = (s {enemies = (map (stepTo (position s)) (enemies s))})

stepTo :: Position->Position->Position
stepTo (toX,toY) (atX,atY) = ((closer atX toX 1),(closer atY toY 1))

closer :: Int -> Int -> Int -> Int
closer at to vel | at < to = at+vel
                 | at > to = at-vel
                 | True    = at

--                           | (atX < toX) = (atX+1,atY)
--                           | (atX > toX) = (atX-1,atY)
--                           | True = (atX,atY)

-- ((atX>toX)?atX-1:(atX<toX)?atX+1:atX,(atY>toY)?atY-1:(atY<toY)?atY+1:atY)

checkCollision :: RobotState -> RobotState
checkCollision s = (s {collided = (collisionBetween (position s) (enemies s))})

collisionBetween :: Position -> [Position] -> Bool
collisionBetween pos (enemy:enemies) = if (pos == enemy) then True else collisionBetween pos enemies
collisionBetween pos [] = False

zapCmd :: RobotState -> RobotState
zapCmd s = (s{zapped=True,laserTo=(movePos (position s) 10 (facing s) (grid s))})

pickCoin :: RobotState -> RobotState
pickCoin s | onCoin s = (s {treasure = position s `delete` treasure s,
                            pocket   = pocket s + 1})
           | True = s

--             do  (s {position = newPos})
--                    where newPos = (movePos (position s) x (facing s))                  

-- | cond p (c1 >> c) (c2 >> c)  ===>  cond p c1 c2 >> c 
-- | repeat p c  ===>  c >> while p c
-- | turnTo d >> direction  ===>  return d


drawBackground w =
  drawInWindowNow w
      (withColor White
         (polygon [(0,0),(xWin,0),(xWin,yWin),(0,yWin)]))


printState :: RobotState -> IO ()
printState s
  = do putStrLn "Ending Robot State:"
       putStrLn ("  Position:  " ++ show (position s))
       putStrLn ("  Facing:    " ++ show (facing s))
       putStrLn ("  Pen Down:  " ++ show (pen s))
       putStrLn ("  Pen Color: " ++ show (color s))
       putStrLn ("  Coins at:  " ++ show (treasure s))
       putStrLn ("  In Pocket: " ++ show (pocket s))

