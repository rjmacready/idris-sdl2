module Graphics.SDL.Render

import Graphics.SDL.Common
import Graphics.SDL.Video
import Graphics.SDL.BlendMode
import Graphics.SDL.Pixels

%include C "SDL2/SDL_render.h"
%include C "csrc/idris_SDL_render.h"
%link C "idris_SDL_render.o"

public
data Renderer = MkRenderer Ptr

public
data Texture = MkTexture Ptr

public
data RendererFlag = RendererSoftware
                  | RendererAccelerated
                  | RendererPresentVSync
                  | RendererTargetTexture

instance Flag Bits32 RendererFlag where
   toFlag RendererSoftware      = 0x00000001
   toFlag RendererAccelerated   = 0x00000002
   toFlag RendererPresentVSync  = 0x00000004
   toFlag RendererTargetTexture = 0x00000008

instance Show RendererFlag where
    show RendererSoftware      = "RendererSoftware"
    show RendererAccelerated   = "RendererAccelerated"
    show RendererPresentVSync  = "RendererPresentVSync"
    show RendererTargetTexture = "RendererTargetTexture"

instance Enumerable RendererFlag where
    enumerate = [RendererSoftware, RendererAccelerated, RendererPresentVSync, RendererTargetTexture]

public
data RendererInfo = MkRendererInfo String (List RendererFlag) (List Bits32) Int Int

public
getNumRenderDrivers : IO Int
getNumRenderDrivers = mkForeign (FFun "SDL_GetNumRenderDrivers" [] FInt)

getSharedWindow : IO Window
getSharedWindow = do
    [| MkWindow (mkForeign (FFun "idris_sharedWindow_window" [] FPtr)) |]

getSharedRenderer : IO Renderer
getSharedRenderer =
    [| MkRenderer (mkForeign (FFun "idris_sharedRenderer_renderer" [] FPtr)) |]

public
createWindowAndRenderer : Int -> Int -> List WindowFlag -> IO (Either String (Window, Renderer))
createWindowAndRenderer w h flags =
    doSDLIf
        (mkForeign (FFun "idris_SDL_createWindowAndRenderer" [FInt, FInt, FBits32] FInt) w h (sumBits flags))
        [| (/*/) getSharedWindow getSharedRenderer |]

public
createRenderer : Window -> Int -> List RendererFlag -> IO Renderer
createRenderer (MkWindow win) index flags =
    [| MkRenderer (mkForeign (FFun "idris_SDL_CreateRenderer" [FPtr, FInt, FBits32] FPtr) win index (sumBits flags)) |]

public
createSoftwareRenderer : Surface -> IO (Either String Renderer)
createSoftwareRenderer (MkSurface surf) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_createSoftwareRenderer" [FPtr] FInt) surf)
        getSharedRenderer

public
getRenderer : Window -> IO (Either String Renderer)
getRenderer (MkWindow win) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getRenderer" [FPtr] FInt) win)
        getSharedRenderer

getTextureFormats : IO (List Bits32)
getTextureFormats = do
    hasNext <- mkForeign (FFun "idris_rendererInfo_hasTextureFormat" [] FInt)
    if (hasNext > 0)
      then do
        bits <- mkForeign (FFun "idris_rendererInfo_getTextureFormat" [] FBits32)
        rest <- getTextureFormats
        return (bits :: rest)
      else do
        return []

getRendererInfo_info : IO RendererInfo
getRendererInfo_info = do
    name <- mkForeign (FFun "idris_rendererInfo_name" [] FString)
    flags <- bitMaskToFlags `map` (mkForeign (FFun "idris_rendererInfo_flags" [] FBits32))
    formats <- getTextureFormats
    width <- mkForeign (FFun "idris_rendererInfo_max_texture_width" [] FInt)
    height <- mkForeign (FFun "idris_rendererInfo_max_texture_height" [] FInt)
    return (MkRendererInfo name flags formats width height)

public
getRenderDriverInfo : Renderer -> IO (Either String RendererInfo)
getRenderDriverInfo (MkRenderer ren) = do
    doSDLIf
        (mkForeign (FFun "idris_SDL_getRendererInfo" [FPtr] FInt) ren)
        getRendererInfo_info

getWidth : IO Int
getWidth = mkForeign (FFun "idris_sharedWidth_int" [] FInt)

getHeight : IO Int
getHeight = mkForeign (FFun "idris_sharedHeight_int" [] FInt)

public
getRendererOutputSize : Renderer -> IO (Either String (Int, Int))
getRendererOutputSize (MkRenderer ren) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getRendererOutputSize" [FPtr] FInt) ren)
        [| (/*/) getWidth getHeight |]

data TextureAccess = TextureAccessStatic
                   | TextureAccessStreaming
                   | TextureAccessTarget

instance Flag Int TextureAccess where
    toFlag TextureAccessStatic    = 0
    toFlag TextureAccessStreaming = 1
    toFlag TextureAccessTarget    = 2

instance Enumerable TextureAccess where
    enumerate = [TextureAccessStatic, TextureAccessStreaming, TextureAccessTarget]

getTexture : IO Texture
getTexture = [| MkTexture (mkForeign (FFun "idris_sharedTexture_texture" [] FPtr)) |]

public
createTexture : Renderer -> Bits32 -> TextureAccess -> Int -> Int -> IO (Either String Texture)
createTexture (MkRenderer ren) format access w h =
    doSDLIf
        (mkForeign (FFun "idris_SDL_createTexture" [FPtr, FBits32, FInt, FInt, FInt] FInt) ren format (toFlag access) w h)
        getTexture

public
createTextureFromSurface : Renderer -> Surface -> IO (Either String Texture)
createTextureFromSurface (MkRenderer ren) (MkSurface surf) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_createTextureFromSurface" [FPtr, FPtr] FInt) ren surf)
        getTexture

public
data TextureInfo = MkTextureInfo Bits32 (Maybe TextureAccess) Int Int

getTextureAccess : IO Int
getTextureAccess = mkForeign (FFun "idris_getSharedFormat_int" [] FInt)

getFormat : IO Bits32
getFormat = mkForeign (FFun "idris_getSharedAccess_int" [] FBits32)

public
queryTexture : Texture -> IO (Either String TextureInfo)
queryTexture (MkTexture txt) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_queryTexture" [FPtr] FInt) txt)
        [| MkTextureInfo getFormat
                         (read `map` getTextureAccess)
                         getWidth
                         getHeight |]

-- | alpha is ignored
public
setTextureColorMod : Texture -> Color -> IO (Maybe String)
setTextureColorMod (MkTexture txt) (MkColor r g b _) =
    doSDL (mkForeign (FFun "SetTextureColorMod" [FPtr, FBits8, FBits8, FBits8] FInt) txt r g b)

--int SDLCALL SDL_GetTextureColorMod(SDL_Texture * texture, Uint8 * r, Uint8 * g, Uint8 * b);

getRed : IO Bits8
getRed = mkForeign (FFun "idris_getRed_uint8" [] FBits8)

getGreen : IO Bits8
getGreen = mkForeign (FFun "idris_getGreen_uint8" [] FBits8)

getBlue : IO Bits8
getBlue = mkForeign (FFun "idris_getBlue_uint8" [] FBits8)

-- | alpha will be set to 1 always. to get alpha use GetTextureAlphaMod
public
getTextureColorMod : Texture -> IO (Either String Color)
getTextureColorMod (MkTexture txt) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getTextureColorMod" [FPtr] FInt) txt)
        [| MkColor getRed
                   getGreen
                   getBlue
                   (return 0xFF) |]

public
setTextureAlphaMod : Texture -> Bits8 -> IO (Maybe String)
setTextureAlphaMod (MkTexture txt) a =
    doSDL (mkForeign (FFun "SDL_SetTextureAlphaMod" [FPtr, FBits8] FInt) txt a)

getAlpha : IO Bits8
getAlpha = mkForeign (FFun "idris_getAlpha_uint8" [] FBits8)

public
getTextureAlphaMod : Texture -> IO (Either String Bits8)
getTextureAlphaMod (MkTexture txt) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getTextureAlphaMod" [FPtr] FInt) txt)
        getAlpha

public
setTextureBlendMode : Texture -> BlendMode -> IO (Maybe String)
setTextureBlendMode (MkTexture txt) mode =
    doSDL (mkForeign (FFun "SDL_SetTextureBlendMode" [FPtr, FBits32] FInt) txt (toFlag mode))

--fixme, display flag and type of flag
--probably also a bad API, just return a maybe and let the user handle it?
extractFlag : (Bits32 -> Maybe a) -> Either String Bits32 -> Either String a
extractFlag f e = e >>= ((maybeToEither ("Unable to read flag")) . f)

public
getTextureBlendMode : Texture -> IO (Either String BlendMode)
getTextureBlendMode (MkTexture txt) =
    (extractFlag read) `map` (doSDLIf
        (mkForeign (FFun "idris_SDL_getTextureBlendMode" [FPtr] FInt) txt)
        (mkForeign (FFun "idris_getBlendMode_mode" [] FBits32)))

public
updateTexture : Texture -> Rect -> Pixels -> Int -> IO (Maybe String)
updateTexture (MkTexture txt) (MkRect x y w h) (MkPixels pix) pitch =
    doSDL (mkForeign (FFun "SDL_UpdateTexture" [FPtr, FInt, FInt, FInt, FInt, FPtr, FInt] FInt) txt x y w h pix pitch)

getPixels : IO Pixels
getPixels =
    [| MkPixels (mkForeign (FFun "idris_getPixels" [] FPtr)) |]

getPitch : IO Int
getPitch =
    mkForeign (FFun "idris_getPitch" [] FInt)

public
lockTexture : Texture -> Rect -> IO (Either String (Pixels, Int))
lockTexture (MkTexture txt) (MkRect x y w h) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_lockTexture" [FPtr, FInt, FInt, FInt, FInt] FInt) txt x y w h)
        [| (/*/) getPixels getPitch |]


public
unlockTexture : Texture -> IO ()
unlockTexture (MkTexture txt) = mkForeign (FFun "SDL_UnlockTexture" [FPtr] FUnit) txt

public
renderTargetSupported : Renderer -> IO Bool
renderTargetSupported (MkRenderer ren) =
    [| fromSDLBool (mkForeign (FFun "SDL_RenderTargetSupported" [FPtr] FInt) ren) |]

public
setRenderTarget : Renderer -> Texture -> IO (Maybe String)
setRenderTarget (MkRenderer ren) (MkTexture txt) =
    doSDL (mkForeign (FFun "SDL_SetRenderTarget" [FPtr, FPtr] FInt) ren txt)

public
getRenderTarget : Renderer -> IO (Either String Texture)
getRenderTarget (MkRenderer ren) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getRenderTarget" [FPtr] FInt) ren)
        [| MkTexture (mkForeign (FFun "idris_sharedTexture" [] FPtr)) |]

public
renderLogicalSize : Renderer -> Int -> Int -> IO (Maybe String)
renderLogicalSize (MkRenderer ren) w h =
    doSDL (mkForeign (FFun "SDL_RenderLogicalSize" [FPtr, FInt, FInt] FInt) ren w h)

--fixme, cannot fail
public
renderGetLogicalSize : Renderer -> IO (Either String (Int, Int))
renderGetLogicalSize (MkRenderer ren) =
    doSDLIf
        ((mkForeign (FFun "idris_SDL_renderGetLogicalSize" [FPtr] FUnit) ren) $> return 1)
        [| (/*/) getWidth getHeight |]

public
renderSetViewport : Renderer -> Rect -> IO (Maybe String)
renderSetViewport (MkRenderer ren) (MkRect x y w h) =
    doSDL (mkForeign (FFun "idris_SDL_renderSetViewport" [FPtr, FInt, FInt, FInt, FInt] FInt) ren x y w h)

getX : IO Int
getX = mkForeign (FFun "idris_render_sharedX_int" [] FInt)

getY : IO Int
getY = mkForeign (FFun "idris_render_sharedY_int" [] FInt)

getRect : IO Rect
getRect = [| MkRect getX
                    getY
                    getWidth
                    getHeight |]

--fixme, cannot fail
public
renderGetViewport : Renderer -> IO (Either String Rect)
renderGetViewport (MkRenderer ren) =
    doSDLIf
        ((mkForeign (FFun "idris_SDL_renderGetViewport" [FPtr] FUnit) ren) $> return 1)
        getRect

public
--int SDLCALL SDL_RenderSetClipRect(SDL_Renderer * renderer, const SDL_Rect * rect);
renderSetClipRect : Renderer -> Rect -> IO (Maybe String)
renderSetClipRect (MkRenderer ren) (MkRect x y w h) =
    doSDL (mkForeign (FFun "idris_SDL_renderSetClipRect" [FPtr, FInt, FInt, FInt, FInt] FInt) ren x y w h)

--fixme, cannot fail
public
renderGetClipRect: Renderer -> IO (Either String Rect)
renderGetClipRect (MkRenderer ren) =
    doSDLIf
        ((mkForeign (FFun "SDL_RenderGetClipRect" [FPtr] FUnit) ren) $> return 1)
        getRect

public
renderSetScale : Renderer -> Int -> Int -> IO (Maybe String)
renderSetScale (MkRenderer ren) scaleX scaleY =
    doSDL (mkForeign (FFun "SDL_RenderSetScale" [FPtr, FInt, FInt] FInt) ren scaleY scaleY)

public
renderGetScale : Renderer -> IO (Either String (Int, Int))
renderGetScale (MkRenderer ren) =
    doSDLIf
        ((mkForeign (FFun "idris_SDL_renderGetScale" [FPtr] FUnit) ren) $> return 1)
        [| (/*/) getX getY |]

public
setRenderDrawColor : Renderer -> Color -> IO (Maybe String)
setRenderDrawColor (MkRenderer ren) (MkColor r g b a) =
   doSDL (mkForeign (FFun "SDL_SetRenderDrawColor" [FPtr, FBits8, FBits8, FBits8, FBits8] FInt) ren r g b a)

public
getRenderDrawColor : Renderer -> IO (Either String Color)
getRenderDrawColor (MkRenderer ren) =
    doSDLIf
        (mkForeign (FFun "idris_SDL_getRenderDrawColor" [FPtr] FInt) ren)
        [| MkColor getRed
                   getGreen
                   getBlue
                   getAlpha |]

public
setRenderDrawBlendMode : Renderer -> BlendMode -> IO (Maybe String)
setRenderDrawBlendMode (MkRenderer ren) mode =
    doSDL (mkForeign (FFun "SDL_SetRenderDrawBlendMode" [FPtr, FBits32] FInt) ren (toFlag mode))

public
getRenderDrawBlendMode : Renderer -> IO (Either String BlendMode)
getRenderDrawBlendMode (MkRenderer ren) =
    (extractFlag read) `map` doSDLIf
        (mkForeign (FFun "idris_SDL_getRenderDrawBlendMode" [FPtr] FInt) ren)
        (mkForeign (FFun "idris_getBlendMode_mode" [] FBits32))

--extern DECLSPEC int SDLCALL SDL_RenderClear(SDL_Renderer * renderer);
public
renderClear : Renderer -> IO (Maybe String)
renderClear (MkRenderer ren) =
    doSDL (mkForeign (FFun "SDL_RenderClear" [FPtr] FInt) ren)

public
renderDrawPoint : Renderer -> Point -> IO (Maybe String)
renderDrawPoint (MkRenderer ren) (MkPoint x y) =
    doSDL (mkForeign (FFun "SDL_RenderDrawPoint" [FPtr, FInt, FInt] FInt) ren x y)

--fixme, use the array version and return an error properly
public total
renderDrawPoints : Renderer -> List Point -> IO (Maybe String)
renderDrawPoints ren (x::xs) = (renderDrawPoint ren x) <$ renderDrawPoints ren xs
renderDrawPoints ren [] = return Nothing

public
renderDrawLine : Renderer -> Point -> Point -> IO (Maybe String)
renderDrawLine (MkRenderer ren) (MkPoint x1 y1) (MkPoint x2 y2) =
    doSDL (mkForeign (FFun "SDL_RenderDrawLine" [FPtr, FInt, FInt, FInt, FInt] FInt) ren x1 y1 x2 y2)

public total
renderDrawLines : Renderer -> List Point -> IO (Maybe String)
renderDrawLines ren (x::y::xs) = (renderDrawLine ren x y) <$ (renderDrawLines ren (y :: xs))
renderDrawLines ren (x::[]) = return Nothing
renderDrawLines ren [] = return Nothing

public
renderDrawRect : Renderer -> Rect -> IO (Maybe String)
renderDrawRect (MkRenderer ren) (MkRect x y w h) =
    doSDL (mkForeign (FFun "idris_SDL_renderDrawRect" [FPtr, FInt, FInt, FInt, FInt] FInt) ren x y w h)

public total
renderDrawRects : Renderer -> List Rect -> IO (Maybe String)
renderDrawRects ren [] = return Nothing
renderDrawRects ren (x::xs) = renderDrawRect ren x <$ renderDrawRects ren xs


public
renderFillRect : Renderer -> Rect -> IO (Maybe String)
renderFillRect (MkRenderer ren) (MkRect x y w h) =
    doSDL (mkForeign (FFun "idris_SDL_renderFillRect" [FPtr, FInt, FInt, FInt, FInt] FInt) ren x y w h)

public total
renderFillRects : Renderer -> List Rect -> IO (Maybe String)
renderFillRects ren [] = return Nothing
renderFillRects ren (x::xs) = renderFillRect ren x <$ renderFillRects ren xs

public
renderCopy : Renderer -> Texture -> Rect -> Rect -> IO (Maybe String)
renderCopy (MkRenderer ren) (MkTexture txt) (MkRect sx sy sw sh) (MkRect dx dy dw dh) =
    doSDL (mkForeign (FFun "idris_SDL_renderCopy" [FPtr, FPtr, FInt, FInt, FInt, FInt, FInt, FInt, FInt, FInt] FInt) ren txt sx sy sw sh dx dy dw dh)

public
data RendererFlip = FlipNone
                  | FlipHorizontal
                  | FlipVertical

instance Flag Bits32 RendererFlip where
    toFlag FlipNone       = 0x00000000
    toFlag FlipHorizontal = 0x00000001
    toFlag FlipVertical   = 0x00000002

public
renderCopyEx : Renderer -> Texture -> Rect -> Rect -> Float -> Point -> RendererFlip -> IO (Maybe String)
renderCopyEx (MkRenderer ren) (MkTexture txt) (MkRect sx sy sw sh) (MkRect dx dy dw dh) angle (MkPoint cx cy) flip =
    doSDL (mkForeign (FFun "idris_SDL_renderCopyEX" [FPtr, FPtr,
                                                      FInt, FInt, FInt, FInt,
                                                      FInt, FInt, FInt, FInt,
                                                      FFloat, FInt, FInt, FBits32]
           FInt) ren txt sx sy sw sh dx dy dw dh angle cx cy (toFlag flip))

public
renderReadPixels : Renderer -> Rect -> {-PixelFormat-}Bits32 -> Int -> IO (Either String Pixels)
renderReadPixels (MkRenderer ren) (MkRect x y w h) format pitch =
    doSDLIf
        (mkForeign (FFun "idris_SDL_renderReadPixels" [FPtr, FInt, FInt, FInt, FInt, FBits32, FInt] FInt) ren x y w h format pitch)
        [| MkPixels (mkForeign (FFun "idris_getSharedPixels" [] FPtr)) |]

public
renderPresent : Renderer -> IO ()
renderPresent (MkRenderer ren) =
    mkForeign (FFun "SDL_RenderPresent" [FPtr] FUnit) ren

public
destroyTexture : Texture -> IO ()
destroyTexture (MkTexture txt) =
    mkForeign (FFun "SDL_DestroyTexture" [FPtr] FUnit) txt

public
destroyRenderer : Renderer -> IO ()
destroyRenderer (MkRenderer ren) =
    mkForeign (FFun "SDL_DestroyRenderer" [FPtr] FUnit) ren

--skipped OGL functions
