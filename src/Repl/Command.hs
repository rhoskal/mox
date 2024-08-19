module Repl.Command where

data Command
  = Quit
  | Error !String
  | None
  | Load
  | Reload
  | TypeOf
  deriving (Eq, Show)
