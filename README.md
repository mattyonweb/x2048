# x2048

A simple Haskell remake of the famous game 2048. Features an AI for the game.

### Installation

(requires Stack):

    stack build

### Execution

x2048 can be played in two modes: single player and AI.

In single player you experience the classic feel of a 2048 game. You can use the keys `2`,`4`,`6`,`8` to move down, left, right, up. To launch this mode, type in your terminal:

    stack exec x2048-exe

`AI` mode lets an AI play for you! Very cybernetic. However don't expect 4096 tiles to pop nor anything special: at the current stage of development, the AI is _sometimes_ (i.e. 4 times out of 100 games) able to reach the 2048 tile; it is much more likely that the game(s) will end reaching the 1024 or maybe the 512 tile. This is something I'm working on, of course. To launch this mode:

     stack exec x2048-exe ts

You'll be asked how many games you want the AI to play; then, it will proceed with the simulations. For every game, you'll be shown the board reached and the value of the top tile.