module MonteCarloGui where

import MonteCarloPi
import Graphics.UI.Gtk hiding (fill,drawPoint)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef
import System.Random

monteCarloGui :: IO ()
monteCarloGui = do initGUI
                   ioref <- newIORef (0,0)
                   Just xml <- xmlNew "montecarlo.glade"
                   window   <- xmlGetWidget xml castToWindow "MonteCarloGui"
                   onDestroy window mainQuit
                   goButton <- xmlGetWidget xml castToButton "go"
                   runs <- xmlGetWidget xml castToEntry "runs"
                   circleArea <- xmlGetWidget xml castToDrawingArea "circle"
                   total <- xmlGetWidget xml castToLabel "total"
                   hits <- xmlGetWidget xml castToLabel "hits"
                   piLabel <- xmlGetWidget xml castToLabel "pi"
                   widgetShowAll window
                   circleDraw <- widgetGetDrawWindow circleArea
                   renderWithDrawable circleDraw paint
                   onExpose circleArea (\x -> do renderWithDrawable circleDraw paint
                                                 return (eventSent x))
                   onClicked goButton (go ioref circleArea (total,hits,piLabel) runs)
                   onEntryActivate runs (go ioref circleArea (total,hits,piLabel) runs)
                   mainGUI

go :: IORef (Int,Int) -> DrawingArea -> (Label,Label,Label) -> Entry -> IO ()
go ref d (t,h,pi) runs = do text <- get runs entryText
                            if ((reads text :: [(Int,String)]) /= []) 
                               then go' (read text) ref d (t,h,pi) 
                               else return ()

go' :: Int -> IORef (Int,Int) -> DrawingArea -> (Label,Label,Label) -> IO ()
go' 0 ref _ (t,h,pi) = do (total,hits) <- readIORef ref
                          set t [ labelText := "Total = " ++ (show total)]
                          set h [ labelText := "Hits = " ++ (show hits)]
                          set pi [ labelText := "Pi is approx " ++ (show (4.0*(fromIntegral hits / fromIntegral total)))]

go' n ref circle (t,h,pi) = do circleDraw <- widgetGetDrawWindow circle
                               (total,hits) <- readIORef ref
                               x <- randomRIO (0,500)
                               y <- randomRIO (0,500)
                               green <- return (isInCircle (fromIntegral x,fromIntegral y) (circleInSquare (fromIntegral 500)))
                               shade <- randomRIO (0.4,1.0)
                               renderWithDrawable circleDraw (drawPoint (x,y) green shade)
                               onExpose circle (\e -> do renderWithDrawable circleDraw (drawPoint (x,y) green shade)
                                                         return (eventSent e))
                               writeIORef ref (total+1,if green then hits+1 else hits)
                               go' (n-1) ref circle (t,h,pi)
                                   

drawPoint :: (Int,Int) -> Bool -> Double -> Render ()
drawPoint (x,y) green shade = do setSourceRGB (if green then 0.0 else shade) (if green then shade else 0.0) 0.0
                                 moveTo (fromIntegral x) (fromIntegral y)
                                 lineTo ((fromIntegral x)-1) ((fromIntegral y)-1)
                                 stroke
                  



