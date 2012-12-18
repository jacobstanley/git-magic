{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Environment
import           System.Environment.Executable
import           System.FilePath hiding (isValid)
import           System.IO (hGetContents)
import           System.Process

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["rebase", commit] -> rebase commit
      ["edit", file]     -> edit file
      _                  -> putStr usage

usage :: String
usage = unlines [
      "Usage: magic <command> [<args>]"
    , ""
    , "The available commands are:"
    , "   rebase <upstream>    Rebase the current branch on top of <upstream>,"
    , "                        but allow editing of the commit messages before"
    , "                        doing the rebase."
    ]

rebase :: FilePath -> IO ()
rebase commit = do
    env'   <- getEnvironment
    self   <- getSelf
    editor <- getEditor

    let cmd = "git rebase -i " ++ commit

    (_,_,_,pid) <- createProcess (shell cmd) {
        env = Just $ env' ++ [ ("GIT_EDITOR", self ++ " edit")
                             , ("OLD_EDITOR", editor) ]
    }

    _ <- waitForProcess pid
    return ()

getEditor :: IO String
getEditor = do
    (_, Just h, _, _) <- createProcess (shell cmd) { std_out = CreatePipe }
    editor <- hGetContents h
    return $ (unwords . words) editor
  where
    cmd = "git config --get core.editor"

getOldEditor :: IO String
getOldEditor = do
    env' <- getEnvironment
    return $ case lookup "OLD_EDITOR" env' of
        Nothing -> error "cannot perform magic without a git editor being set"
        Just x  -> x

getSelf :: IO String
getSelf = do
    spath <- getScriptPath
    return $ case spath of
        Executable path -> "\"" ++ path ++ "\""
        RunGHC path     -> "runghc \"" ++ path ++ "\""
        Interactive     -> error "cannot perform magic from GHCi"

edit :: FilePath -> IO ()
edit path = do
    --putStrLn $ "Applying sorcery to " ++ path
    case takeFileName path of
        "git-rebase-todo" -> editTodo
        "COMMIT_EDITMSG"  -> editCommit
        _                 -> error $ "not sure how to apply sorcery to " ++ path
  where
    gitPath = joinPath $ takeWhile (/= ".git") (splitDirectories path) ++ [".git"]
    magicPath = gitPath ++ "/rebase-merge/magic-rebase-todo"

    editTodo = do
        -- Create magic todo from git todo
        todo <- T.readFile path
        T.writeFile magicPath (magic todo)

        -- Edit magic todo list
        editor <- getOldEditor
        let cmd = unwords $ map quoted [editor, magicPath]
        (_,_,_,pid) <- createProcess (proc "sh" ["-c", cmd])
        _ <- waitForProcess pid

        -- Create git todo from magic todo
        mtodo <- T.readFile magicPath
        T.writeFile path (unmagic mtodo)

        return ()

    editCommit = do
        mtodo <- T.readFile magicPath
        let (subject, mtodo') = nextSubject mtodo
        T.writeFile magicPath mtodo'

        msg <- T.readFile path
        let msg' = T.unlines $ [subject] ++ drop 1 (T.lines msg)
        T.writeFile path msg'

type GitTodo = T.Text
type MagicTodo = T.Text
type MagicSubject = T.Text

magic :: GitTodo -> MagicTodo
magic = T.unlines . map (T.unwords . drop 1) . filter isValid . map T.words . T.lines
  where
    -- strip empty lines and comments
    isValid :: [T.Text] -> Bool
    isValid []      = False
    isValid ("#":_) = False
    isValid _       = True

unmagic :: MagicTodo -> GitTodo
unmagic = T.unlines . map ("reword " `T.append`) . T.lines

nextSubject :: MagicTodo -> (MagicSubject, MagicTodo)
nextSubject todo = (latest, items')
  where
    latest = (T.unwords . drop 1 . T.words) next
    items' = T.unlines rest
    (next:rest) = T.lines todo

quoted :: String -> String
quoted xs = "\"" ++ xs ++ "\""

