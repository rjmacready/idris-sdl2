
all:
	~/.cabal/bin/idris --ibcsubdir dist driver.idr -o driver && ./driver

#ctest:
#	gcc -g -O0 -c idris_SDL_video.c -lSDL2

clean:
	rm -rf driver *~ *.ibc *.o