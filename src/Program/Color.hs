module Program.Color where

import System.Console.ANSI

withColor :: Color -> IO a -> IO a
withColor col action = do
  setSGR [SetColor Foreground Vivid col]
  res <- action
  setSGR [SetDefaultColor Foreground]
  return res
