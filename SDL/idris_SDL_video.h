#ifndef IDRIS_SDL_VIDEO_H
#define IDRIS_SDL_VIDEO_H

#include "SDL2/SDL_rect.h"
#include "SDL2/SDL_video.h"

int idris_SDL_getDisplayBounds(int index);
int idris_SDL_getDisplayBounds_x();
int idris_SDL_getDisplayBounds_y();
int idris_SDL_getDisplayBounds_w();
int idris_SDL_getDisplayBounds_h();

int idris_sharedDisplayMode(int displayIndex, int (displayGetter) (int, SDL_DisplayMode*));
int idris_sharedDisplayMode2(int displayIndex, int modeIndex, int(displayGEtter)(int, int, SDL_DisplayMode*));
Uint32 idris_sharedDisplayMode_format();
int idris_sharedDisplayMode_w();
int idris_sharedDisplayMode_h();
int idris_sharedDisplayMode_refresh_rate();
void* idris_sharedDisplayMode_driverdata();

int idris_SDL_getDisplayMode(int displayIndex, int modeIndex);
int idris_SDL_getDesktopDisplayMode(int displayIndex);
int idris_SDL_getCurrentDisplayMode(int displayIndex);

int idris_SDL_CreateWindow(const char* title, int x, int y, int w, int h, Uint32 flags);
SDL_Window* idris_SDL_CreateWindow_window();

#endif /*IDRIS_SDL_VIDEO_H*/
