{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (void)
import           Data.Either
import qualified Data.Text as T
import           Options.Applicative
import           Prelude hiding (FilePath)
import           Shelly
import           System.Random.Shuffle
import qualified Text.Parsec as P

---

data Flag = Set T.Text | Random (Maybe FilePath)

flags :: Parser Flag
flags = ((Set . T.pack) <$> strOption
        (long "set" <> short 's' <> metavar "FILE"
         <> help "Set the wallpaper given."))
        <|> (Random <$> (flag' Nothing
         (long "random" <> short 'r'
          <> help "Randomly picks an image file from a directory.")
         *> optional (fromText . T.pack <$> argument str (metavar "DIR"))))

setwall :: T.Text -> Sh ()
setwall fname = run_ "hsetroot" ["-fill", fname]

setRandom :: FilePath -> Sh ()
setRandom dir = do
  pics  <- ls dir >>= liftIO . shuffleM . filter isImg
  when (not $ null pics) (setwall . toTextIgnore $ head pics)

isImg :: FilePath -> Bool
isImg file = isRight . P.parse img "" $ toTextIgnore file

img :: P.Parsec T.Text () ()
img = void $ P.many (P.noneOf ".") *> P.char '.' *> P.choice exts
  where exts = map P.string ["jpg", "jpeg", "png"]

work :: Flag -> Sh ()
work (Set file)        = setwall file
work (Random Nothing)  = pwd >>= setRandom
work (Random (Just d)) = setRandom d

main :: IO ()
main = execParser opts >>= shelly . work
  where opts = info (helper <*> flags)
               (fullDesc <> header "setwall - Set a Linux wallpaper.")
