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

data Player =
     X | O

instance Eq Player where
   (==) X X = True
   (==) O O = True
   (==) _ _ = False

instance Show Player where
         show X = "X"
         show O = "O"

data GamePhase = 
          Playing
          | Win Player
          | Stalemate

GameState : Type
GameState = (Player, Vect 9 (Maybe Player))

initState : GameState
initState = (X, [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing])

xyToFlat : (Int, Int) -> Int
xyToFlat (x, y) = y * 3 + x

flatToXy : Int -> (Int, Int)
flatToXy flat = (flat `modInt` 3, flat `divInt` 3)

mapState : GameState -> (Int -> Maybe Player -> IO ()) -> IO ()
mapState g action = 
         let (_, board) = g in
                do walk 0 board
             where
                walk : Int -> Vect _ (Maybe Player) -> IO ()
                walk _ [] = do 
                            return ()
                walk i (x :: xs) = do 
                             action i x
                             walk (i+1) xs

             
testVictory : Vect 9 (Maybe Player) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Player
testVictory board pt0 pt1 pt2 =
           let (i0, i1, i2) = (integerToFin (cast (xyToFlat pt0)) (fromIntegerNat 9), 
                               integerToFin (cast (xyToFlat pt1)) (fromIntegerNat 9), 
                               integerToFin (cast (xyToFlat pt2)) (fromIntegerNat 9)) in
               do
                i0 <- i0
                i1 <- i1
                i2 <- i2
                case ([| (index i0 board) == (index i1 board) |],
                     [| (index i1 board) == (index i2 board) |]) of
                    (Just True, Just True) =>
                          (index i0 board)                          
                    (_, _) => Nothing
            
victoryDiag0 : Vect 9 (Maybe Player) -> Maybe Player
victoryDiag0 board = testVictory board (0, 0) (1, 1) (2, 2)
 
victoryDiag1 : Vect 9 (Maybe Player) -> Maybe Player
victoryDiag1 board = testVictory board (2, 0) (1, 1) (0, 2)
                                         
victoryRow : Vect 9 (Maybe Player) -> Int -> Maybe Player
victoryRow board row = testVictory board (0, row) (1, row) (2, row)

victoryCol : Vect 9 (Maybe Player) -> Int -> Maybe Player
victoryCol board col = testVictory board (col, 0) (col, 1) (col, 2)


firstOf : Maybe Player -> Maybe Player -> Maybe Player
firstOf Nothing a = a
firstOf a _ = a

victory : Vect 9 (Maybe Player) -> Maybe Player
victory board = 
        firstOf (victoryRow board 0) $ 
        firstOf (victoryRow board 1) $
        firstOf (victoryRow board 2) $
        firstOf (victoryCol board 0) $ 
        firstOf (victoryCol board 1) $
        firstOf (victoryCol board 2) $
        firstOf (victoryDiag0 board)
                (victoryDiag1 board)


getGamePhase : GameState -> GamePhase
getGamePhase g =
             let (_, board) = g in
                 let victor = victory board in
                     case victor of
                          Nothing =>
                                  let someEmpty = Vect.find (\x => case x of
                                                                   Nothing => True
                                                                   _ => False) board in
                                  case someEmpty of
                                       Nothing => Stalemate
                                       Just _ => Playing
                          Just v => Win v


screenToXy : (Int, Int) -> Maybe (Int, Int)
screenToXy (sx, sy) = if sx > 300 || sy > 300 then
                         Nothing
                      else
                        Just (sx `divInt` 100, sy `divInt` 100)


nextPlayer : Player -> Player
nextPlayer x =
           case x of
                X => O
                O => X

makePlay : GameState -> (Int, Int) -> Maybe GameState
makePlay prev idx = 
         let flat = integerToFin (cast (xyToFlat idx)) (fromIntegerNat 9) in
         case flat of 
              Nothing => Nothing
              Just flat => 
                   let (current, board) = prev in
                   case (index flat board) of
                        Just _ => Nothing
                        Nothing => 
                                Just (nextPlayer current, 
                                     replaceAt flat (Just current) board)


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
                  
drawXat : Renderer -> (Int, Int) -> IO ()
drawXat rend (x, y) = 
             let (x, y) = (x * 100, y * 100) in
             do
                renderDrawLine rend (MkPoint x y) (MkPoint (x+100) (y+100))
                renderDrawLine rend (MkPoint (x+100) y) (MkPoint x (y+100))
                return ()

-- Actually draws a rhombus
drawOat : Renderer -> (Int, Int) -> IO ()
drawOat rend (x, y) =
             let (x, y) = (x * 100, y * 100) in
             do
                renderDrawLine rend (MkPoint (x+50) y) (MkPoint (x+100) (y+50))
                renderDrawLine rend (MkPoint (x+100) (y+50)) (MkPoint (x + 50) (y+100))
                renderDrawLine rend (MkPoint (x+50) (y + 100)) (MkPoint x (y+50))
                renderDrawLine rend (MkPoint x (y + 50)) (MkPoint (x+50) y)
                return ()


renderScene : Window -> Renderer -> GameState -> IO ()
renderScene w rend g = do
                       setRenderDrawColor rend (MkColor 0 0 0 255)
                       renderClear rend
                       setRenderDrawColor rend (MkColor 255 255 255 255)
                       renderDrawLine rend (MkPoint 0 100) (MkPoint 300 100)
                       renderDrawLine rend (MkPoint 0 200) (MkPoint 300 200)

                       renderDrawLine rend (MkPoint 100 0) (MkPoint 100 300)
                       renderDrawLine rend (MkPoint 200 0) (MkPoint 200 300)
                       
                       mapState g (\i => \p =>
                                   case p of
                                        Nothing => do 
                                                return ()
                                        Just X =>
                                               let upperLeft = flatToXy i in
                                               do 
                                                  drawXat rend upperLeft
                                                  return ()
                                        Just O =>
                                               let upperLeft = flatToXy i in
                                               do 
                                                  drawOat rend upperLeft
                                                  return ()) 
                       
                       renderPresent rend


makeTurn : Int -> Int -> GameState -> GameState
makeTurn sx sy gstate =
     let pos = screenToXy (sx, sy) in
     case pos of 
          Nothing => gstate
          Just pos =>
               let newState = makePlay gstate pos in
                   case newState of
                        Nothing => gstate
                        Just newState => newState


mutual

        renderAndEvents : Window -> Renderer -> GameState -> IO ()
        renderAndEvents w r g = do
                          renderScene w r g
                          case (getGamePhase g) of
                               Win p =>  do
                                   putStrLn $ (show p) ++ " won!" 
                                   return ()
                               Stalemate => do
                                    putStrLn "stalemate!"
                                    return ()
                               _ => do 
                                    eventLoopTest w r g

        eventLoopTest : Window -> Renderer -> GameState -> IO ()
        eventLoopTest w r g = do
                      event <- pollEvent
                      case event of
                           Left err => do
                                delay 10

                                renderAndEvents w r g
                           Right (timestamp, event) =>
                                 case event of
                                      QuitEvent => do
                                                return ()
                                      MouseButtonEvent ButtonDown b c d e f sx sy => do            

                                                       renderAndEvents w r (makeTurn sx sy g)
                                      _ => do
                                        renderAndEvents w r g


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
              eventLoopTest window renderer initState
              return ()
         None => do
              putStr "error on doWindow\n"
              return ()
