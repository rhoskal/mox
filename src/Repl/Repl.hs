module Repl.Repl (interpret) where

import Data.Char qualified as C
import Repl.Command
import System.Console.Isocline
import System.Directory qualified as Dir
import System.Exit (exitSuccess)

interpret :: IO ()
interpret = do
  styleDef "ic-prompt" "ansi-maroon"
  putFmtLn header
  tmpPath <- (++ ".mox/") <$> Dir.getTemporaryDirectory
  _ <- Dir.createDirectoryIfMissing False tmpPath
  setHistory (tmpPath ++ "history.txt") 200
  _ <- enableAutoTab True
  interaction
  where
    header =
      " __  __              \n"
        ++ "|  \\/  | _____  __  \n"
        ++ "| |\\/| |/ _ \\ \\/ /\n"
        ++ ("| |  | | (_) >  <    " ++ welcome)
        ++ ("|_|  |_|\\___/_/\\_\\   " ++ help)
    welcome = "welcome to the Mox repl\n"
    help = "type :? for help, and :q to quit\n"

interaction :: IO ()
interaction = do
  s <- readlineEx "" (Just completer) (Just highlighter)
  eval $ readCommand s
  interaction

completer :: CompletionEnv -> String -> IO ()
completer cEnv input = do
  completeFileName cEnv input Nothing [".", "/usr/local"] []
  completeWord cEnv input Nothing wordCompletions

wordCompletions :: String -> [Completion]
wordCompletions input =
  completionsFor
    (map C.toLower input)
    ["func1", "func2", "func3"]

highlighter :: String -> Fmt
highlighter = tokenize
  where
    tokenize [] = []
    tokenize s@('#' : _) =
      let (t, ds) = span (/= '\n') s
       in style "#408700" (plain t) ++ tokenize ds
    tokenize s@(c : cs)
      | C.isAlpha c =
          let (t, ds) = span C.isAlpha s
           in ( if t `elem` ["fun", "struct", "var", "val"]
                  then style "keyword" t -- builtin style
                  else
                    if t `elem` ["return", "if", "then", "else"]
                      then style "control" t -- builtin style
                      else
                        if t `elem` ["int", "double", "char", "void"]
                          then style "#00AFAF" t -- or use specific colors
                          else plain t -- never lose input, all original characters must be present!
              )
                ++ tokenize ds
      | C.isDigit c =
          let (t, ds) = span C.isDigit s
           in style "number" t ++ tokenize ds
      | otherwise = plain [c] ++ tokenize cs -- never lose input

eval :: Command -> IO ()
eval = \case
  Quit -> exitSuccess
  Clear -> historyClear
  None -> return ()
  Eval _ -> putStrLn "eval"
  Help -> putStrLn commandHelp
  Load _ -> putStrLn "load"
  Reload -> putStrLn "reload"
  TypeOf -> putStrLn "type"
  Error err -> putStrLn err
