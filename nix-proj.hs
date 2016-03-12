module Main where

import Data.ConfigFile
import Data.List (isPrefixOf,intercalate)
import System.Environment (getArgs,getEnv,setEnv)

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import Control.Monad (filterM, when, unless)
import qualified Control.Monad.State as S
import System.IO.Error hiding (catch)

import System.FilePath
import System.PosixCompat ( createSymbolicLink
                          , getSymbolicLinkStatus
                          , isSymbolicLink )
import System.Process (system)
import System.Exit (die,ExitCode(..))

type Name = String
type Email = String

data Author = Author { authorName :: Name, authorEmail :: Email }
data Config = Config { repo :: FilePath
                     , author :: Author }
data Project = Project { config :: Config
                       , name :: Name
                       , workspace :: FilePath }
type ProjectS a = S.StateT Project IO a

getAuthor :: ProjectS Author
getAuthor = do
  p <- S.get
  return (author $ config p)

getRepo :: ProjectS FilePath
getRepo = do
  p <- S.get
  return (repo $ config p)

getName :: ProjectS Name
getName = do
  p <- S.get
  return (name p)

getWorkspace :: ProjectS FilePath
getWorkspace = do
  p <- S.get
  return (workspace p)

io :: IO a -> ProjectS a
io = S.liftIO

readConfig :: FilePath -> IO Config
readConfig path = do
  raw <- readFile path
  let config = do
        conf <- readstring emptyCP raw
        [repo]
          <- mapM (get conf "DEFAULT")
               ["repo"]
        [name,email]
          <- mapM (get conf "author")
               ["name","email"]
        return Config { repo = repo
                      , author = Author name email }
  case config of
    Left err -> error $ show err
    Right conf -> return conf

listDirectories :: FilePath -> IO [FilePath]
listDirectories fp = do
  fs <- getDirectoryContents fp
  let absFs = map (\f -> fp </> f) $ filter spec fs
  filterM doesDirectoryExist absFs
  where spec fi = fi /= "."
               && fi /= ".."
               && fi /= ".git"

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

mkProject :: ProjectS ()
mkProject = do
  repoLocation <- (</>) <$> getRepo <*> getName
  workLocation <- getWorkspace
  io $ createDirectoryIfMissing True repoLocation
  let repoFiles = baseOf repoLocation
      workFiles = baseOf workLocation
  mapM_ (\f -> io $ ensureFile f) repoFiles
  mapM_ (\t -> io $ lnProjFile t) $ zip repoFiles workFiles

  where

    ensureFile fp =
      doesFileExist fp >>=
        \exist ->
          unless exist $ createFile fp

    lnProjFile (pfp,cfp) = do
      exist <- doesFileExist cfp
      when exist $ do
        symlink <- ((fmap isSymbolicLink).getSymbolicLinkStatus) cfp
        if symlink then
          renameFile cfp pfp
        else
          removeFile cfp
      createSymbolicLink pfp cfp

    baseOf base =
      map (\f -> base </> f)
        [ "default.nix"
        , "shell.nix"
        , "release.nix"
        ]

gitCmd :: String -> ProjectS ExitCode
gitCmd cmd = do
  repo <- getRepo
  io $ system $
    intercalate " " [ "git"
                    , "--work-tree"
                    , repo
                    , "--git-dir"
                    , repo </> ".git"
                    , cmd ]

gitAuthor :: ProjectS ExitCode
gitAuthor = do
  author <- getAuthor
  gitCmd $ "config user.name " ++ authorName author
  gitCmd $ "config user.email" ++ authorEmail author

checkRepo :: ProjectS Bool
checkRepo = do
  c <- gitCmd "status"
  return (ExitSuccess == c)

addChangesAndCommit :: ProjectS ()
addChangesAndCommit = do
  name <- getName
  gitCmd ("add " ++ name)
  gitCmd ("commit -m 'Updated: "++ name ++ "'")
  return ()

main = do
  home <- getHomeDirectory
  cwd <- getCurrentDirectory
  let confDir = home </> ".config" </> "nix-proj"
      config = confDir </> "conf"
      createFile path = writeFile path ""
  createDirectoryIfMissing True confDir
  c@(Config repoPath auth) <- readConfig config
  repo <- replaceTilde repoPath
  when (repo `isPrefixOf` cwd) $ die "Not allowed to operate under the nix files project repo"
  let project = Project (Config repo auth) (takeFileName cwd) cwd
  S.runStateT gitAuthor project
  args <- getArgs
  case args of
    ["ls"] -> do
      dirs <- listProjectNames repo
      mapM_ (\p -> putStrLn $ "" ++ p) dirs
    ["ci"] -> do
      S.runStateT addChangesAndCommit project
      return ()
    ["mk"] -> do
      S.runStateT mkProject project
      return ()
    ["st"] -> do
      (bool,_) <- S.runStateT checkRepo project
      return ()
    [] ->
      putStrLn "Nothing.."
    xs ->
      putStrLn $ "Unknown sequence: " ++ intercalate " " xs
  return ()
  where
    replaceTilde s =
      case splitDirectories s of
        "~":rel -> do
                   homeDir <- getHomeDirectory
                   return $ joinPath $ homeDir : rel
        _ -> makeAbsolute s
    listProjectNames repo = map takeFileName <$> listDirectories repo
