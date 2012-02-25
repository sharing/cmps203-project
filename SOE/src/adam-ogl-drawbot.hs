
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent

distill s = map act s
	where act c = do
		case c of
			'w' -> t 1 0
			'a' -> t 0 1
			's' -> t (-1) 0
			'd' -> t 0 (-1)
			'e' -> r (-1)
			'q' -> r 1
			_ -> return ()
		where
			t x y = do
				renderPrimitive Lines $ do
					vertex $ Vertex3 (0::GLfloat) 0 0
					vertex $ Vertex3 (x::GLfloat) y 0
				translate $ Vector3 x y (0::GLfloat)
			r h =
				rotate (30*h) $ Vector3 (0::GLfloat) 0 1

compile source =
	mapM_ (\cmd -> do cmd ; scale q q q)
		$ take 10000
		$ cycle
		$ distill source
	where q = 0.999 :: GLfloat
	
shell pRef = loop
  where loop = do
          source <- getLine
          pRef $= compile source
          loop

display pRef = do
  program <- get pRef
  clear [ColorBuffer]
  rotate 0.05 $ Vector3 (0::GLfloat) 0 1
  preservingMatrix $ do
  	scale sc sc sc
  	program
  addTimerCallback 20 $ postRedisplay Nothing
  swapBuffers
    where sc = 0.25 :: GLfloat

main = do
  pRef <- newIORef $ compile "qwsasasdqwe"
  forkOS $ shell pRef
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 800 800
  createWindow progName
  displayCallback $= display pRef
  mainLoop
