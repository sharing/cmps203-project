import Array
import Data.List.Split --to use, must run in command prompt window "cabal install split"
import System.Exit

main = sim initState

data RobotState 
  = RobotState 
        { position  :: Position
        , facing    :: Direction
		, exit		:: Bool
        }
     deriving Show

initState :: RobotState
initState = RobotState 
				{ position 	= (0,0)
                , facing  	= North
				, exit		= False
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
			 | Exit
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
run_c (Exit) s = do (s {exit = True})
run_c (No_Op) s = s

parse :: [String] -> Command
parse (c:[]) = case c of
				"left" -> (Lt)
				"right" -> (Rt)
				"exit" -> (Exit)
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
  = if (exit s) == True 
		then exitSuccess
		else
			do 
			putStrLn "Current Robot State:"
			putStrLn ("  Position:  " ++ show (position s))
			putStr ("  Facing:    " ++ show (facing s) ++ "\n\n> ")
	   


