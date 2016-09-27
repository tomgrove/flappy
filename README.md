# flappy
A zx spectrum horizontal scrolling demo in the style of flappy bird.

Somes notes:

The basic approach to scrolling is the same as Uridium ( http://zxspectrumgames.blogspot.co.uk/2009/07/zx-spectrum-game-uridium-zx-spectrum.html ) a spectrum game ported from the C64. At the time  ( and subsequently, by the lonely souls who still care about such things ) it was held up as an impressive technical feat. This was because the spectrum has no hardware support for scrolling and sprites so all rendering has to be accomplished in software against very tight timing constraints.

The approach taken ia to construct a character cell (8x8 pixel) tilemap very similar to the tilemaps used by NES, etc. Because there is no hardware support for scrolling, each tile is pre-scrolled ahead of time, taking into account its neighbours. Writing the tilemap is very fast and exploits the an architetural feature of the z80 that makes the stack the fastest way of reading or writing memory. There is also a lot of unrolling. Sprites are custom tiles written over the background tiles.


