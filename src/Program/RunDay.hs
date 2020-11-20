module Program.RunDay (runDay) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import System.Directory (doesFileExist)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Bool -> String -> IO ()
runDay inputParser partA partB verbose inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then (liftIO $ readFile inputFile)
        else throwError $ "I couldn't read the input! I was expecting it to be at " ++ inputFile
    case (parseOnly inputParser . pack $ fileContents) of
      Left e -> throwError $ "Parser failed to read input. Error " ++ e
      Right i -> do
        when verbose $ do
          liftIO $ putStrLn "Parser output:"
          liftIO . putStrLn . show $ i
        return i
  processInput input
  where
    processInput (Left x) = putStrLn x
    processInput (Right i) = do
      putStrLn "Part A:"
      catch (print $ partA i) (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part A!" >> (when verbose $ print m))
      putStrLn "Part B:"
      catch (print $ partB i) (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part B!" >> (when verbose $ print m))
