module GuiTest where

import Graphics.UI.Gtk
import Data.IORef

gui :: IO ()
gui = do initGUI
         ioref <- newIORef 0
         window <- windowNew
         button <- buttonNew
         set window [ containerBorderWidth := 10,
                      containerChild := button ]
         set button [ buttonLabel := "Button clicked 0 time(s)" ]
         onClicked button (whatToDoWith ioref button)
         onDestroy window mainQuit
         widgetShowAll window
         mainGUI

whatToDoWith :: (IORef Int) -> Button -> IO ()
whatToDoWith ioref button = do num <- readIORef ioref
                               set button [ buttonLabel := ("Button clicked " ++ (show (num+1)) ++ " time(s)") ]
                               writeIORef ioref (num+1)
