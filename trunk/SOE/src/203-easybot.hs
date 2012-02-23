import Array
import Data.List.Split

main = sim initState

data RobotState 
  = RobotState 
        { position  :: Position
        , facing    :: Direction
        }
     deriving Show

initState :: RobotState
initState = RobotState 
				{ position = (0,0)
                , facing   = North
                }

type Position = (Int,Int)

data Direction = North | East | South | West
     deriving (Eq,Show,Enum)

right,left :: Direction -> Direction

right d = toEnum (succ (fromEnum d) `mod` 4)
left  d = toEnum (pred (fromEnum d) `mod` 4)

data Command = Lt
             | Rt
             | Fd Int
			 | Bk Int
			 | No_Op

data Program = Single Command
             | Sequence Program Program

sim::RobotState-> IO ()
sim s = do cmdStr <- getLine
           let cmd = parse (splitOn " " cmdStr)
           let s' = run_c cmd s
           printState s'
           sim s' 

run_c :: Command -> RobotState -> RobotState
run_c (Fd x) s = do  (s {position = newPos})
                     where newPos = (movePos (position s) x (facing s))
run_c (Bk x) s = do  (s {position = newPos})
                     where newPos = (movePos (position s) x (facing s))
run_c (Lt) s = do (s {facing = left (facing s)})
run_c (Rt) s = do (s {facing = right (facing s)})
run_c (No_Op) s = s

parse :: [String] -> Command
parse (c:[]) = case c of
				"left" -> (Lt)
				"right" -> (Rt)
				"exit" -> error "\nGoodbye!"
				_ -> (No_Op)
parse (cmd:val:tail) = case cmd of
				"fwd" -> (Fd (read $ val :: Int))
				"backward" -> (Bk (read $ val :: Int))
				_ -> (No_Op)
				

movePos :: Position -> Int -> Direction -> Position
movePos (x,y) v d
  = case d of
      North -> (x,y+v)
      South -> (x,y-v)
      East  -> (x+v,y)
      West  -> (x-v,y)

printState :: RobotState -> IO ()
printState s
  = do putStrLn "Current Robot State:"
       putStrLn ("  Position:  " ++ show (position s))
       putStrLn ("  Facing:    " ++ show (facing s))


