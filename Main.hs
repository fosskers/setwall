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

flags :: Parser (Sh ())
flags = ((setwall . T.pack) <$> strOption
        (long "set" <> short 's' <> metavar "FILE"
         <> help "Set the wallpaper given."))
        <|> flag' setRandom
        (long "random" <> short 'r'
         <> help "Randomly picks an image file from the current directory.")

setwall :: T.Text -> Sh ()
setwall fname = run_ "hsetroot" ["-fill", fname]

setRandom :: Sh ()
setRandom = do
  pics  <- pwd >>= ls >>= liftIO . shuffleM . filter isImg
  when (not $ null pics) (setwall . toTextIgnore $ head pics)

isImg :: FilePath -> Bool
isImg file = isRight . P.parse img "" $ toTextIgnore file

img :: P.Parsec T.Text () ()
img = void $ P.many (P.noneOf ".") *> P.char '.' *> P.choice exts
  where exts = map P.string ["jpg", "jpeg", "png"]

main :: IO ()
main = execParser opts >>= shelly
  where opts = info (helper <*> flags)
               (fullDesc <> header "setwall - Set a Linux wallpaper.")
