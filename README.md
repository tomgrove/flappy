# flappy

![Flappy Bird!](/flappy.gif)

A zx spectrum horizontal scrolling demo in the style of flappy bird.

Somes notes:

The basic approach to scrolling is the same as Uridium ( http://zxspectrumgames.blogspot.co.uk/2009/07/zx-spectrum-game-uridium-zx-spectrum.html ) a spectrum game ported from the C64. At the time  ( and subsequently, by the lonely souls who still care about such things ) it was held up as an impressive technical feat. This was because the spectrum has no hardware support for scrolling and sprites so all rendering has to be accomplished in software against very tight timing constraints.

The approach taken ia to construct a character cell (8x8 pixel) tilemap very similar to the tilemaps used by NES, etc. Because there is no hardware support for scrolling, each tile is pre-scrolled ahead of time, taking into account its neighbours. Writing the tilemap is very fast and exploits the an architetural feature of the z80 that makes the stack the fastest way of reading or writing memory. There is also a lot of unrolling. Sprites are custom tiles written over the background tiles.

This is further improved to exploit the nature of the tilemap in the case of flappy bird. We add cheap commands for skipping empty tiles and cheat with respect to the pipe stems- these use the same pattern repeated vertically. These changes make it possible to run at 50hz as well as supporting some limited parallax scrolling. The cloud image is scrolled and pushed to the screen directly using the stack. This is then written over by the tilemap using a mask where it overlaps with the stems.

collisions are just perfomred ( badly ) against the tilemap, there is some simple input and motion that half-heartedly follows the original mobile game. Space bar /key flaps.

There is no sound.

This shoudl assemble directly into a snapshot using the Zeus Assembler  ( http://www.desdes.com/products/oldfiles/index.htm ). The resulting snapshot can be played on a real-spectrum using  https://code.google.com/archive/p/otla/downloads which can create a wav file directly from the snapshot.  

The code is a bit ugly and has far too much indirection. For speed, the tilemap actually contains the opcode as well as the operands for each tile. This makes the tilemap bloated and doesn't help all that much. About half the frame is spent copying the tilemap into the command list. This is clearly silly - should render from the tilemap directly.


