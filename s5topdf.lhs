#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

The [Shelly](https://github.com/yesodweb/Shelly.hs) library has some utilities
for making it easier to use Haskell as a shell scripting language.

\begin{code}
import           Shelly
import           Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.Text.Lazy as T
import           System.Environment
import           Prelude hiding (FilePath)
import           Filesystem.Path
\end{code}

The `main` function picks out two parameters and tries to make one a `Int`. If
any of that fails, it prints the usage message and bails.

\begin{code}
main :: IO ()
main = shelly $ verbosely $ do
    args <- liftIO $ getArgs
    case args of
        [slides', url, pdf] | all C.isNumber slides' -> do
            downloadSlides (read slides') url >>= convert (fromString pdf)
            echo . T.pack $ "Wrote PDF to " ++ pdf
        otherwise -> echo usage
\end{code}

This is the usage/help message.

\begin{code}
usage :: T.Text
usage = "\
    \usage: s5topdf.lhs [slides] [url] \n\
    \ \n\
    \  slides is the number of slides in the slideshow.\n\
    \  url    is the URL to access the slideshow at.\n"
\end{code}

This downloads the slides and returns the file names that were generated.

\begin{code}
downloadSlides :: Int -> String -> ShIO [FilePath]
downloadSlides slideCount baseUrl = do
  forM_ inputs $ \(url, file) -> webkit2png_ file url
  return files'
  where
      baseUrl' = T.pack $ baseUrl ++ "#slide"
      range    = [0..slideCount]
      urls     = map (T.append baseUrl' . T.pack . show)    range
      files    = map (T.append "slide-" . T.pack . show)    range
      files'   = map (fromText . flip T.append "-full.png") files
      inputs   = zip urls files
\end{code}

This executes the webkit2png command, downloading a URL into a file.

This command could fail (if `webkit2png` isn't available), and that should
probably propagate and short-circuit the rest of the process.

\begin{code}
webkit2png_ filename url = do
  script <- which "webkit2png"
  case script of
      Nothing      -> echo "ERROR: webkit2png not installed."
      Just script' -> do
        s <- toTextWarn script'
        python26_ s [ "--fullsize"
                    , "--filename", filename
                    , url
                    ]
\end{code}

This runs a script using python2.6. My default Python is 2.7, but I haven't
bothered bothered installing `pyobjc` for it.

\begin{code}
python26_ script args = run_ "python2.6" (script:args)
\end{code}

This takes the list of PNG files to convert and generates the PDF.

\begin{code}
convert :: FilePath -> [FilePath] -> ShIO ()
convert pdf pngs = run_ "convert" =<< mapM toTextWarn (pngs ++ [pdf])
\end{code}

This is a utility for converting from String to FilePath.

\begin{code}
fromString :: String -> FilePath
fromString = fromText . T.pack 
\end{code}

\begin{code}
-- vim: set filetype=haskell:
\end{code}
