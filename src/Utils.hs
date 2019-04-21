module Utils
(
  Direction (Up, Down, Left, Right, NoDir)
)
where

data Direction = Up | Down | Left | Right | NoDir deriving (Show, Eq)
