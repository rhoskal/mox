module Repl.Command
  ( Command (..),
    commandHelp,
    readCommand,
  )
where

data Command
  = Quit
  | Clear
  | None
  | Error !String
  | Eval !String
  | Help
  | Load !FilePath
  | Reload
  | TypeOf
  deriving (Show)

commandHelp :: String
commandHelp = "show help"

readCommand :: String -> Command
readCommand = \case
  ":q" -> Quit
  "clear" -> Clear
  "" -> None
  ":?" -> Help
  ":l" -> Load "some_file.mox"
  ":r" -> Reload
  ":t" -> TypeOf
  err -> Error $ "error in command: " ++ err
