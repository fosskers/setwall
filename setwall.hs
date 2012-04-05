import System.IO (openTempFile,hPutStrLn,hClose)
import System.Random (getStdGen,randomR)
import System.Environment (getArgs)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)
import System.Console.GetOpt
import System.Cmd (system)
import Data.Char (toLower)
import System.Directory ( getCurrentDirectory
                        , getDirectoryContents
                        , getHomeDirectory
                        , renameFile
                        )

data Flag = Save | RandomWall | Help

options :: [OptDescr Flag]
options = [ Option ['s'] ["save"]   (NoArg Save)       xDesc
          , Option ['r'] ["random"] (NoArg RandomWall) rDesc
          , Option ['h'] ["help"]   (NoArg Help)       hDesc
          ]
    where xDesc = "Change the current wallpaper, and also edit ~/.xinitrc" ++
                  "\nto to save the change."
          rDesc = "Randomly picks an image file from the current directory" ++
                  "\nand sets it as the wallpaper."
          hDesc = "Display this help message."

usageMsg :: String
usageMsg = "Usage : setwall [OPTION] (image file)"

-- Where .xinitrc should be.
pathToXinitrc :: IO String
pathToXinitrc = do
  home <- getHomeDirectory
  return (home ++ "/.xinitrc")

command :: String
command = "hsetroot -fill "

main = do
  args <- getArgs
  opts <- processOpts args
  case opts of
    ([Save],(fname:_))     -> saveToXinitrc fname
    ([RandomWall],dirName) -> setRandomWall dirName
    ([Help],_)     -> putStrLn $ usageInfo usageMsg options                              
    ([],(fname:_)) -> changeWallpaper $ command ++ fname
    (_,[])         -> argError "No image file given."
    (_,_)          -> argError "Flag related error."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> argError "Bad flag used."

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

saveToXinitrc :: String -> IO ()
saveToXinitrc fname = do
  rcFile   <- pathToXinitrc                
  rawLines <- readFile rcFile
  fullPath <- getFullPath fname
  let fixedLines = swapLine fullPath $ lines rawLines
  (tempFile,handle) <- openTempFile "." "temp.txt"
  hPutStrLn handle $ unlines fixedLines
  hClose handle
  renameFile tempFile rcFile
  changeWallpaper $ command ++ fullPath

getFullPath :: String -> IO String
getFullPath fname = do
  pwd <- getCurrentDirectory
  return (pwd ++ "/" ++ fname)

swapLine :: String -> [String] -> [String]
swapLine path = map (\line -> if line == "" || (head $ words line) /= target
                              then line
                              else command ++ path)
    where target = "hsetroot"

changeWallpaper :: String -> IO ()
changeWallpaper file = system file >> return ()

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