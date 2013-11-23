module SDL.Video

import SDL.Common
import SDL.SDL
import SDL.Rect
import Data.Bits

%include C "SDL2/SDL_video.h"
%include C "SDL/idris_SDL_video.h"
%link C "idris_SDL_video.o"

public
data Window = mkWindow Ptr

public
data Surface = mkSurface Ptr

instance Show Window where
    show x = "Window"

public
GetNumVideoDrivers : IO Int
GetNumVideoDrivers = mkForeign (FFun "SDL_GetNumVideoDrivers" [] FInt) 

public 
GetVideoDriver : Int -> IO String
GetVideoDriver index = mkForeign (FFun "SDL_GetVideoDriver" [FInt] FString) index

public
VideoInit : String -> IO Int
VideoInit driver_name = mkForeign (FFun "SDL_VideoInit" [FString] FInt) driver_name

public
VideoQuit : IO ()
VideoQuit = mkForeign (FFun "SDL_VideoQuit" [] FUnit)

public
GetCurrentVideoDriver : IO String
GetCurrentVideoDriver = mkForeign (FFun "SDL_GetCurrentVideoDriver" [] FString)

public
GetNumVideoDisplays : IO Int
GetNumVideoDisplays = mkForeign (FFun "SDL_GetNumVideoDisplays" [] FInt)

public 
GetDisplayName : Int -> IO String
GetDisplayName displayIndex = mkForeign (FFun "SDL_GetDisplayName" [FInt] FString) displayIndex

checkGetDisplayBounds : Int -> IO Int
checkGetDisplayBounds index = mkForeign (FFun "idris_SDL_getDisplayBounds" [FInt] FInt) index

getDisplayBounds_x : IO Int
getDisplayBounds_x = mkForeign (FFun "idris_SDL_getDisplayBounds_x" [] FInt)

getDisplayBounds_y : IO Int
getDisplayBounds_y = mkForeign (FFun "idris_SDL_getDisplayBounds_y" [] FInt)

getDisplayBounds_w : IO Int
getDisplayBounds_w = mkForeign (FFun "idris_SDL_getDisplayBounds_w" [] FInt)

getDisplayBounds_h : IO Int
getDisplayBounds_h =  mkForeign (FFun "idris_SDL_getDisplayBounds_h" [] FInt)

public
GetDisplayBounds : Int -> IO (Either String Rect)
GetDisplayBounds index = do
    isValid <- checkGetDisplayBounds index
    if (isValid /= 0) 
      then do
        err <- GetError
        return $ Left err
      else do
        rect <- [| mkRect getDisplayBounds_x
                          getDisplayBounds_y
                          getDisplayBounds_w
                          getDisplayBounds_h |]
        return $ Right rect 
          
        
public 
GetNumDisplayModes : Int -> IO Int
GetNumDisplayModes index = mkForeign (FFun "SDL_GetNumDisplayModes" [FInt] FInt) index

public
data DisplayMode = mkDisplayMode Bits32 Int Int Int Ptr

instance Show DisplayMode where
    show (mkDisplayMode format w h refresh_rate _) =
        "DisplayMode " ++ (show format) ++ " " ++ (show w) ++ " " ++ (show h) ++ " " ++ show (refresh_rate)

sharedDisplayMode_format : IO Bits32
sharedDisplayMode_format =
  mkForeign (FFun "idris_sharedDisplayMode_format" [] FBits32)
             
sharedDisplayMode_w : IO Int
sharedDisplayMode_w =
  mkForeign (FFun "idris_sharedDisplayMode_w" [] FInt)
  
sharedDisplayMode_h : IO Int
sharedDisplayMode_h =
  mkForeign (FFun "idris_sharedDisplayMode_h" [] FInt)

sharedDisplayMode_refresh_rate : IO Int
sharedDisplayMode_refresh_rate =
  mkForeign (FFun "idris_sharedDisplayMode_refresh_rate" [] FInt)
  
sharedDisplayMode_driverdata : IO Ptr
sharedDisplayMode_driverdata =
  mkForeign (FFun "idris_sharedDisplayMode_driverdata" [] FPtr)


checkGetDisplayMode : Int -> Int -> IO Int
checkGetDisplayMode displayIndex modeIndex =
    mkForeign (FFun "idris_SDL_getDisplayMode" [FInt, FInt] FInt) displayIndex modeIndex

getSharedDisplayMode : Int -> IO (Either String DisplayMode)
getSharedDisplayMode isValid =
    if (isValid /= 0)
      then do
        err <- GetError
        return $ Left err
      else do
        mode <- [| mkDisplayMode sharedDisplayMode_format
                                 sharedDisplayMode_w
                                 sharedDisplayMode_h
                                 sharedDisplayMode_refresh_rate
                                 sharedDisplayMode_driverdata |]
        return $ Right mode
public
GetDisplayMode : Int -> Int -> IO (Either String DisplayMode)
GetDisplayMode displayIndex modeIndex = do
    isValid <- checkGetDisplayMode displayIndex modeIndex
    getSharedDisplayMode isValid
        
checkGetDesktopDisplayMode : Int -> IO Int
checkGetDesktopDisplayMode displayIndex = 
      mkForeign (FFun "idris_SDL_getDesktopDisplayMode" [FInt] FInt) displayIndex

GetDesktopDisplayMode : Int -> IO (Either String DisplayMode)
GetDesktopDisplayMode displayIndex = do
    isValid <- checkGetDesktopDisplayMode displayIndex 
    getSharedDisplayMode isValid

public
GetCurrentDisplayMode : Int -> IO (Either String DisplayMode)
GetCurrentDisplayMode displayIndex = do
    isValid <- checkGetDesktopDisplayMode displayIndex 
    getSharedDisplayMode isValid

public
GetClosestDisplayMode : Int -> DisplayMode -> IO (Either String DisplayMode)
GetClosestDisplayMode displayIndex (mkDisplayMode format w h hz ddata) = do
    isValid <- (mkForeign (FFun "idris_SDL_getClosestDisplayMode" [FInt, FBits32, FInt, FInt, FInt, FPtr] FInt) displayIndex format w h hz ddata)
    getSharedDisplayMode isValid
  
public
GetWindowDisplayIndex : Window -> IO (Maybe Int)
GetWindowDisplayIndex (mkWindow ptr) = do
    index <- mkForeign (FFun "SDL_GetWindowDisplayIndex" [FPtr] FInt) ptr
    return $ case index of
      -1 => Nothing
      n  => Just n

--code clones
--returns an error message on failure
public
SetWindowDisplayMode : Window -> DisplayMode -> IO (Maybe String)
SetWindowDisplayMode (mkWindow ptr) (mkDisplayMode format w h hz ddata) = do
    success <- mkForeign (FFun "idris_SDL_setWindowDisplayMode" [FPtr, FBits32, FInt, FInt, FInt, FPtr] FInt) ptr format w h hz ddata
    if (success /= 0)
      then do
        errorString <- GetError
        return $ Just errorString
      else
        return Nothing

public
GetWindowDisplayMode : Window -> IO (Either String DisplayMode)
GetWindowDisplayMode (mkWindow ptr) = do
    isValid <- mkForeign (FFun "idris_SDL_getWindowDisplayMode" [FPtr] FInt) ptr
    getSharedDisplayMode isValid

--shouldnt fail on account of a null window because we never allow a
--    a window to be null within idris
public
GetWindowPixelFormat : Window -> IO Bits32
GetWindowPixelFormat (mkWindow ptr) = 
    mkForeign (FFun "SDL_GetWindowPixelFormat" [FPtr] FBits32) ptr 

data WindowFlags = WindowFullscreen
                 | WindowOpengl
                 | WindowShown
                 | WindowHidden
                 | WindowBorderless
                 | WindowResizable
                 | WindowMinimized
                 | WindowMaximized
                 | WindowInputGrabbed
                 | WindowInputFocus
                 | WindowMouseFocus
                 | WindowFullscreenDesktop
                 | WindowForeign
                   
instance Flag WindowFlags where
    toBits WindowFullscreen         = 0x00000001
    toBits WindowOpengl             = 0x00000002
    toBits WindowShown              = 0x00000004
    toBits WindowHidden             = 0x00000008
    toBits WindowBorderless         = 0x00000010
    toBits WindowResizable          = 0x00000020
    toBits WindowMinimized          = 0x00000040
    toBits WindowMaximized          = 0x00000080
    toBits WindowInputGrabbed       = 0x00000100
    toBits WindowInputFocus         = 0x00000200
    toBits WindowMouseFocus         = 0x00000400
    toBits WindowFullscreenDesktop  = (toBits WindowFullscreen) `or32` 0x00001000
    toBits WindowForeign            = 0x00000800

--we check if the window was created successfully
checkCreateWindow :  String -> Int -> Int -> Int -> Int -> Bits32 -> IO Int
checkCreateWindow title x y w h flags =
  (mkForeign (FFun "idris_SDL_CreateWindow" [FString, FInt, FInt, FInt, FInt, FBits32] FInt) title x y w h flags)
  
getCreateWindow : IO Window
getCreateWindow = mkWindow `map` (mkForeign (FFun "idris_sharedWindow" [] FPtr))

public
CreateWindow : String -> Int -> Int -> Int -> Int -> Bits32 -> IO (Either String Window)
CreateWindow title x y w h flags = do
    status <- checkCreateWindow title x y w h flags
    if (status == 0)
      then do
        errorString <- GetError
        return $ Left errorString
      else do
        ptr <- getCreateWindow
        return $ Right ptr

public
CreateWindowFrom : Ptr -> IO (Either String Window)
CreateWindowFrom ptr = do
    success <- mkForeign (FFun "idris_SDL_createWindowFrom" [FPtr] FInt) ptr
    if (success /= 0)
      then do
        errorString <- GetError
        return $ Left errorString
      else do
        ptr <- mkForeign (FFun "idris_SDL_sharedWindow" [] FPtr)
        return $ Right (mkWindow ptr)
        
--is 0 a legit return value here?
public
GetWindowID : Window -> IO Bits32
GetWindowID (mkWindow ptr) =
    mkForeign (FFun "SDL_GetWindowID" [FPtr] FBits32) ptr

public
GetWindowFromID : Bits32 -> IO (Either String Window)
GetWindowFromID id = do
    success <- mkForeign (FFun "idris_SDL_getWindowFromID" [FBits32] FInt) id
    if (success /= 0)
      then do
        errorString <- GetError
        return $ Left errorString
      else do
        ptr <- mkForeign (FFun "idris_sharedWindow" [] FPtr)
        return $ Right (mkWindow ptr)

--this function might be pure
public
GetWindowFlags : Window -> IO Bits32
GetWindowFlags (mkWindow ptr) =
    mkForeign (FFun "SDL_GetWindowFlags" [FPtr] FBits32) ptr

public
SetWindowTitle : Window -> String -> IO ()
SetWindowTitle (mkWindow ptr) title =
    mkForeign (FFun "SDL_SetWindowTitle" [FPtr, FString] FUnit) ptr title

--this function might be pure
public
GetWindowTitle : Window -> IO String
GetWindowTitle (mkWindow ptr) =
    mkForeign (FFun "SDL_GetWindowTitle" [FPtr] FString) ptr

--does this function indicate when failing?  
public
SetWindowIcon : Window -> Surface -> IO ()
SetWindowIcon (mkWindow win) (mkSurface surf) =
    mkForeign (FFun "SDL_SetWindowIcon" [FPtr, FPtr] FUnit) win surf

