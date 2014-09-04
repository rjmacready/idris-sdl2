module Main

import Graphics.SDL.Common
import Graphics.SDL.SDL
import Graphics.SDL.Timer
import Graphics.SDL.Rect
import Graphics.SDL.Video
import Graphics.SDL.Mouse
import Graphics.SDL.Clipboard
import Graphics.SDL.Events
import Graphics.SDL.BlendMode
import Graphics.SDL.CPUInfo
import Graphics.SDL.Bits
import Graphics.SDL.GameController
import Graphics.SDL.Render
import Utils.Map

doInit : IO ()
doInit = do
    init <- init [InitEverything]
    case init of
        Just err => putStrLn err
        Nothing => return ()

testRenderer : Renderer -> IO ()
testRenderer renderer = do
    info <- getRenderDriverInfo renderer
    case info of
        Right (MkRendererInfo name flags formats maxwidth maxheight) =>
            putStrLn $ join [ name
                            , " "
                            , show flags
                            , " "
                            , show formats
                            , " "
                            , show maxwidth
                            , " "
                            , show maxheight
                            ]
        Left err => putStrLn ("ERROR " ++ err)

               
t : Type
t = Maybe (Window, Renderer) 

doWindow : IO t
doWindow = do
    x <- createWindow "test" 600 600 600 600 [WindowShown]
    case x of
        Left err => do
                 putStr err
                 return Nothing
        Right win => do
                  putStr "what\n"
                  rend <- createRenderer win 0 [RendererSoftware]
                  testRenderer rend
                  putStr "ok\n"
                  return (Just (win, rend))

renderScene : Window -> Renderer -> IO ()
renderScene w rend = do
                       --putStr "."
                       setRenderDrawColor rend (MkColor 0 0 0 255)
                       renderClear rend
                       setRenderDrawColor rend (MkColor 255 255 255 255)
                       renderDrawLine rend (MkPoint 0 100) (MkPoint 300 100)
                       renderDrawLine rend (MkPoint 0 200) (MkPoint 300 200)

                       renderDrawLine rend (MkPoint 100 0) (MkPoint 100 300)
                       renderDrawLine rend (MkPoint 200 0) (MkPoint 200 300)
                       
                       renderPresent rend
                       --putStr "*"


eventLoopTest : Window -> Renderer -> IO ()
eventLoopTest w r = do
    event <- pollEvent
    case event of
        Left err => do
            delay 10
            eventLoopTest w r
        Right (timestamp, event) =>
            case event of
                QuitEvent => do
                          return ()
                MouseButtonEvent ButtonDown b c d e f g h => do
                          putStr "mouse bt "
                          putStr (show b)
                          putStr " "
                          putStr (show c)
                          putStr " "
                          putStr (show d)
                          putStr " "
                          putStr (show e)
                          putStr " "
                          putStr (show f)
                          putStr " "
                          putStr (show g)
                          putStr " "
                          putStr (show h)
                          putStr "\n"
                          renderScene w r
                          eventLoopTest w r
                _ => do 
                        renderScene w r
                        eventLoopTest w r

{-
    setClipboardText "clipboard2"
    clip <- getClipboardText
    putStrLn $ "Clipboard: " ++ (show clip)
    delay 1000
-}

main : IO ()
main = do
    doInit
    i <- getInit
    putStrLn $ show i
    r <- doWindow
    case r of 
         Just (window, renderer) => do
              num <- getDisplayBounds 0
              mode <- getDisplayMode 0 0
              putStrLn $ show num
              putStrLn $ show mode
              putStr "start event loop\n"
              eventLoopTest window renderer
         None => do
              putStr "error on doWindow\n"
              return ()

{-
num <- getDisplayBounds 0
mode <- getDisplayMode 0 0
putStrLn $ show num
putStrLn $ show mode                                    
-}
