module Main where

import Command.Add qualified as AddCmd
import Command.Build qualified as BuildCmd
import Command.Clean qualified as CleanCmd
import Command.Deps qualified as DepsCmd
import Command.Docs qualified as DocsCmd
import Command.Format qualified as FormatCmd
import Command.Lint qualified as LintCmd
import Command.Lsp qualified as LspCmd
import Command.New qualified as NewCmd
import Command.Publish qualified as PublishCmd
import Command.Remove qualified as RemoveCmd
import Command.Repl qualified as ReplCmd
import Command.Run qualified as RunCmd
import Command.Search qualified as SearchCmd
import Command.Test qualified as TestCmd
import Control.Monad (join)
import Data.Version qualified as Version
import Options.Applicative
import Paths_mox qualified as Meta

data Command
  = Add AddCmd.Options
  | Build BuildCmd.Options
  | Clean
  | Deps DepsCmd.Options
  | Docs DocsCmd.Options
  | Format FormatCmd.Options
  | Lint LintCmd.Options
  | Lsp LspCmd.Options
  | New NewCmd.Options
  | Publish PublishCmd.Options
  | Remove RemoveCmd.Options
  | Repl
  | Run RunCmd.Options
  | Search SearchCmd.Options
  | Test TestCmd.Options
  deriving (Show)

main :: IO ()
main = join $ customExecParser (prefs showHelpOnEmpty) parseOpts

parseOpts :: ParserInfo (IO ())
parseOpts =
  info (helper <*> parseVersion <*> parseCmd) $
    fullDesc
      <> footer "For more information, please visit https://github.com/rhoskal/mox"

parseVersion :: Parser (a -> a)
parseVersion =
  infoOption
    (prettyVersion Meta.version)
    ( short 'V'
        <> long "version"
        <> help "Show version"
    )
  where
    prettyVersion :: Version.Version -> String
    prettyVersion = (++) "Mox v" . Version.showVersion

parseCmd :: Parser (IO ())
parseCmd =
  hsubparser
    ( command "add" (info (pure AddCmd.run) (progDesc "Add new project dependencies"))
        <> command "build" (info (pure BuildCmd.run) (progDesc "Add new project dependencies"))
        <> command "clean" (info (pure CleanCmd.run) (progDesc "Clean build artifacts"))
        <> command "deps" (info (pure DepsCmd.run) (progDesc "Work with dependencies"))
        <> command "docs" (info (pure DocsCmd.run) (progDesc "Render HTML documentation"))
        <> command "format" (info (pure FormatCmd.run) (progDesc "Format source code"))
        <> command "lint" (info (pure LintCmd.run) (progDesc "Lint source code"))
        <> command "lsp" (info (pure LspCmd.run) (progDesc "Run the language server"))
        <> command "new" (info (pure NewCmd.run) (progDesc "Create a new project"))
        <> command "publish" (info (pure PublishCmd.run) (progDesc "Publish the project"))
        <> command "remove" (info (pure RemoveCmd.run) (progDesc "Remove project dependencies"))
        <> command "repl" (info (pure ReplCmd.run) (progDesc "Run interactive interpreter"))
        <> command "run" (info (pure RunCmd.run) (progDesc "Run the project"))
        <> command "search" (info (pure SearchCmd.run) (progDesc "Search for a dependency"))
        <> command "test" (info (pure TestCmd.run) (progDesc "Run the project tests"))
    )
