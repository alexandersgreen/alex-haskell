module Poker where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Data.IORef
import Cards

type Pics = (Surface,Surface,Surface,Surface)

poker :: IO ()
poker = do withImageSurfaceFromPNG "images/back.png" (\back ->
             withImageSurfaceFromPNG "images/front.png" (\front ->
               withImageSurfaceFromPNG "images/clubs.png" (\clubs ->
                 withImageSurfaceFromPNG "images/hearts.png" (\hearts ->
                   withImageSurfaceFromPNG "images/spades.png" (\spades ->
                     withImageSurfaceFromPNG "images/diamonds.png" (\diamonds ->
                       do initGUI
                          ioref <- newIORef 0
                          Just xml <- xmlNew "poker.glade"
                          window   <- xmlGetWidget xml castToWindow "Alex's Haskell Poker"
                          onDestroy window mainQuit
                          dealMenu <- xmlGetWidget xml castToMenuItem "Deal Item"
                          quitMenu <- xmlGetWidget xml castToMenuItem "Quit Item"
                          imageLeft <- xmlGetWidget xml castToDrawingArea "Left"
                          imageMiddle <- xmlGetWidget xml castToDrawingArea "Middle"
                          imageRight <- xmlGetWidget xml castToDrawingArea "Right"
                          quitButton <- xmlGetWidget xml castToButton "Quit Button" 
                          dealButton <- xmlGetWidget xml castToButton "Deal Button"
                          text <- xmlGetWidget xml castToLabel "Text"
                          onActivateLeaf quitMenu (quitAction window)
                          onActivateLeaf dealMenu (dealAction imageLeft imageMiddle imageRight text ioref front (clubs,hearts,spades,diamonds))
                          onClicked quitButton (quitAction window)
                          onClicked dealButton (dealAction imageLeft imageMiddle imageRight text ioref front (clubs,hearts,spades,diamonds))
                          widgetShowAll window
                          left <- widgetGetDrawWindow imageLeft
                          renderWithDrawable left (noCard back)
                          onExpose imageLeft (\x -> do renderWithDrawable left (noCard back)
                                                       return (eventSent x))
                          middle <- widgetGetDrawWindow imageMiddle
                          renderWithDrawable middle (noCard back)
                          onExpose imageMiddle (\x -> do renderWithDrawable middle (noCard back)
                                                         return (eventSent x))
                          right <- widgetGetDrawWindow imageRight
                          renderWithDrawable right (noCard back)
                          onExpose imageRight (\x -> do renderWithDrawable right (noCard back)
                                                        return (eventSent x))         
                          mainGUI))))))

quitAction :: Window -> IO ()
quitAction window = widgetDestroy window

dealAction :: DrawingArea -> DrawingArea -> DrawingArea -> Label -> (IORef Int) -> Surface -> Pics -> IO ()
dealAction l m r t ioref bg ss = do deals <- readIORef ioref
                                    ((lc,mc,rc),text) <- dealCards
                                    left <- widgetGetDrawWindow l
                                    renderWithDrawable left (drawCard lc bg ss)
                                    onExpose l (\x -> do renderWithDrawable left (drawCard lc bg ss)
                                                         return (eventSent x))
                                    middle <- widgetGetDrawWindow m
                                    renderWithDrawable middle (drawCard mc bg ss)
                                    onExpose m (\x -> do renderWithDrawable middle (drawCard mc bg ss)
                                                         return (eventSent x))
                                    right <- widgetGetDrawWindow r
                                    renderWithDrawable right (drawCard rc bg ss)
                                    onExpose r (\x -> do renderWithDrawable right (drawCard rc bg ss)
                                                         return (eventSent x))
                                    set t [ labelText := "Deal " ++ (show (deals + 1)) ++ ": " ++ text]
                                    writeIORef ioref (deals + 1)

noCard :: Surface -> Render ()
noCard surface = do setSourceSurface surface 0.0 0.0
                    paint
            
--220x337
--50x50
drawCard :: Card -> Surface -> (Pics) -> Render ()
drawCard (Card (v,suit)) surf (c,h,s,d) = do setSourceSurface surf 0.0 0.0
                                             paint
                                             setSourceSurface (getSuit suit (c,h,s,d)) 0.0 0.0
                                             paint
                                             setSourceSurface (getSuit suit (c,h,s,d)) 165.0 282.0 
                                             paint
                                             setSourceRGB (getRed suit) 0.0 0.0
                                             setFontSize 100.0
                                             moveTo 50.0 200.0
                                             showText (show v)
                                             stroke

getSuit :: Suit -> Pics -> Surface
getSuit Clubs (c,_,_,_) = c
getSuit Hearts (_,h,_,_) = h
getSuit Spades (_,_,s,_) = s
getSuit Diamonds (_,_,_,d) = d

getRed :: Suit -> Double
getRed Clubs = 0.0
getRed Spades = 0.0
getRed x = 1.0

