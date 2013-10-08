module HelloGlade where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

helloGlade :: IO ()
helloGlade = do initGUI
                Just xml <- xmlNew "hello_glade.glade"
                window   <- xmlGetWidget xml castToWindow "Hello Glade"
                onDestroy window mainQuit
                label <- xmlGetWidget xml castToLabel "name label"
                textBox <- xmlGetWidget xml castToEntry "text entry"
                closeButton <- xmlGetWidget xml castToButton "close button"
                applyButton <- xmlGetWidget xml castToButton "apply button"
                onClicked closeButton (doCloseButton window)
                onClicked applyButton (doApplyButton True textBox label)
                onEntryActivate textBox (doApplyButton False textBox label)
                widgetShowAll window
                mainGUI

doCloseButton :: Window -> IO ()
doCloseButton window = do putStrLn "Close Button Pressed"
                          widgetDestroy window

doApplyButton :: Bool -> Entry -> Label -> IO ()
doApplyButton button textBox label = do if button then putStrLn "Apply Button Pressed"
                                                  else putStrLn "Text Box: Enter Pressed"
                                        text <- get textBox entryText
                                        set label [ labelText := "Hello " ++ text ]


