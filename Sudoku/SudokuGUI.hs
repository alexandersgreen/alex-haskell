module SudokuGUI where
-- A GUI for Sudoku
-- GUI by Alexander S. Green

import Sudoku -- by Graham Hutton
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
--import Data.IORef

sudokuGUI :: IO ()
sudokuGUI = do initGUI
               --ioref <- newIORef 0
               --get the Glade xml file
               Just xml <- xmlNew "sudoku.glade"
               --get the windows and dialog boxes
               window   <- xmlGetWidget xml castToWindow "sudoku"
               about    <- xmlGetWidget xml castToDialog "aboutdialog"
               dialog   <- xmlGetWidget xml castToDialog "dialog"
               --set the programme to quit when closing the main window
               onDestroy window mainQuit
               --get other widgets
               --menubar (only need leaves)
               solveMenu <- xmlGetWidget xml castToMenuItem "solvemenu"
               quitMenu  <- xmlGetWidget xml castToMenuItem "quitmenu"
               aboutMenu <- xmlGetWidget xml castToMenuItem "aboutmenu"
               --1-9 buttons, clear button, solve button, status bar, and text view
               button1 <- xmlGetWidget xml castToButton "1button"
               button2 <- xmlGetWidget xml castToButton "2button"
               button3 <- xmlGetWidget xml castToButton "3button"
               button4 <- xmlGetWidget xml castToButton "4button"
               button5 <- xmlGetWidget xml castToButton "5button"
               button6 <- xmlGetWidget xml castToButton "6button"
               button7 <- xmlGetWidget xml castToButton "7button"
               button8 <- xmlGetWidget xml castToButton "8button"
               button9 <- xmlGetWidget xml castToButton "9button"
               buttonClear <- xmlGetWidget xml castToButton "clearbutton"
               buttonSolve <- xmlGetWidget xml castToButton "solvebutton"
               statusBar <- xmlGetWidget xml castToLabel "statuslabel"
               textView <- xmlGetWidget xml castToTextView "textview"
               --sudoku board pieces (81 images, 9 sub tables, 1 main table)
               sudokuTable <- xmlGetWidget xml castToTable "sudokutable"
               tl <- xmlGetWidget xml castToTable "tl"
               tm <- xmlGetWidget xml castToTable "tm"
               tr <- xmlGetWidget xml castToTable "tr"
               ml <- xmlGetWidget xml castToTable "ml"
               mm <- xmlGetWidget xml castToTable "mm"
               mr <- xmlGetWidget xml castToTable "mr"
               bl <- xmlGetWidget xml castToTable "bl"
               bm <- xmlGetWidget xml castToTable "bm"
               br <- xmlGetWidget xml castToTable "br"
               --dialog box buttons and label
               aboutLabel   <- xmlGetWidget xml castToLabel "aboutlabel"
               dialogButton <- xmlGetWidget xml castToButton "okbutton"
               aboutButton  <- xmlGetWidget xml castToButton "closebutton"
               --set textbox colors, sudoku frame colors,  and about label text size
               widgetModifyBase textView StateNormal (Color 0 0 0)
               widgetModifyText textView StateNormal (Color 65535 65535 65535)
               newFont <- fontDescriptionFromString "Courier Bold 30"
               widgetModifyFont aboutLabel (Just newFont)
               widgetModifyBg sudokuTable StateNormal (Color 0 0 0)
               widgetModifyBg tl StateNormal (Color 0 0 0)
               widgetModifyBg tl StateActive (Color 0 0 65535)
               --set actions
               onActivateLeaf solveMenu (widgetShowAll dialog)
               onClicked buttonSolve (widgetShowAll dialog)
               onActivateLeaf quitMenu (widgetDestroy window)
               onActivateLeaf aboutMenu (widgetShowAll about)
               onClicked dialogButton (widgetHideAll dialog) 
               onClicked aboutButton (widgetHideAll about)
               --set to display the main window
               widgetShowAll window
               --start the GUI
               mainGUI


