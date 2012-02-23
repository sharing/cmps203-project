import Array

main = sim initState

data Command = Lt
             | Rt
             | Fd Int

data Program = Single Command
             | Sequence Program Program

sim::RobotState->IO ()
sim s = do cmdStr <- getLine
           let cmd = (parse cmdStr)
           let s' = run_c cmd s
           printState s'
           sim s' 

run_c::Command->RobotState->RobotState
run_c (Fd x) s = do  (s {position = newPos})
                     where newPos = (movePos (position s) (facing s))
run_c (Lt) s = do (s {facing = left (facing s)})
run_c (Rt) s = do (s {facing = right (facing s)})

parse::String->Command
parse str = case str of
              "left" -> (Lt)
              "fwd" -> (Fd 10)

movePos :: Position -> Direction -> Position
movePos (x,y) d
  = case d of
      North -> (x,y+1)
      South -> (x,y-1)
      East  -> (x+1,y)
      West  -> (x-1,y)

printState :: RobotState -> IO ()
printState s
  = do putStrLn "Ending Robot State:"
       putStrLn ("  Position:  " ++ show (position s))
       putStrLn ("  Facing:    " ++ show (facing s))

data RobotState 
  = RobotState 
        { position  :: Position
        , facing    :: Direction
        }
     deriving Show

initState :: RobotState
initState = RobotState { position = (0,0)
                , facing   = North
                }

type Position = (Int,Int)

data Direction = North | East | South | West
     deriving (Eq,Show,Enum)

right,left :: Direction -> Direction

right d = toEnum (succ (fromEnum d) `mod` 4)
left  d = toEnum (pred (fromEnum d) `mod` 4)
