import System.Directory (getCurrentDirectory, getDirectoryContents,
                         getHomeDirectory, renameFile)    
import System.Environment (getArgs)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)
import System.Console.GetOpt
import System.Cmd (system)
import Data.Char (toLower)
import System.Random

data Flag = RewriteXinitrc | RandomWall

options :: [OptDescr Flag]
options = [ Option ['x'] [] (NoArg RewriteXinitrc) xDesc
          , Option ['r'] ["random"] (NoArg RandomWall) rDesc
          ]
    where xDesc = "Change the current wallpaper, and also edit ~/.xinitrc" ++
                  "\nto to save the change."
          rDesc = "Randomly picks an image file from the current directory" ++
                  "\nand sets it as the wallpaper."

usageMsg :: String
usageMsg = "Usage : setwall [OPTION] (image file)"

-- Where .xinitrc should be.
pathToXinitrc :: IO String
pathToXinitrc = do
  home <- getHomeDirectory
  return (home ++ "/.xinitrc")

tempFilename :: String
tempFilename = "temp.lol"  -- Could this already exist?

-- The keyword that we're looking for.
target :: String
target = "hsetroot"

command :: String
command = "hsetroot -fill "

main = do
  args <- getArgs
  opts <- processOpts args
  case opts of
    ([RewriteXinitrc],(filename:_)) -> putStrLn "Not ready yet."
    ([RandomWall],dirName) -> setRandomWall dirName
    ([],(filename:_)) -> changeWallpaper $ command ++ filename
    (_,[])            -> argError "No image file given."
    (_,_)             -> argError "Flag related error."

{-
main = do
  (fname:_) <- getArgs
  rcFile    <- pathToXinitrc                
  rawLines  <- readFile rcFile
  fullPath  <- getFullPath fname
  changeWallpaper $ command ++ fullPath  -- Set the new wallpaper now.
  -- Also set it in the .xinitrc file to save the change.
  let fixedLines = swapLine fullPath $ lines rawLines
  writeFile tempFilename $ unlines fixedLines
  renameFile tempFilename rcFile
-}

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> argError "Bad flag used."

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

setRandomWall :: [String] -> IO ()
setRandomWall dir
    | null dir  = setwall "./"
    | otherwise = setwall $ head dir
    where setwall dir = do
            contents <- getDirectoryContents dir
            gen      <- getStdGen
            let pics = filter isPic contents
            case pics of
              [] -> putStrLn "No valid images files in this directory."
              _  -> do
                let (pos,gen') = randomR (0, length pics - 1) gen
                    newWall    = pics !! pos
                changeWallpaper $ command ++ dir ++ newWall
              where isPic file = file =~ "([.]jpg$|[.]jpeg$|[.]png$)" :: Bool

getFullPath :: String -> IO String
getFullPath fname = do
  pwd <- getCurrentDirectory
  return (pwd ++ "/" ++ fname)

changeWallpaper :: String -> IO ()
changeWallpaper file = system file >> return ()

swapLine :: String -> [String] -> [String]
swapLine path = map (\line -> if line == "" || (head $ words line) /= target
                              then line
                              else command ++ path)