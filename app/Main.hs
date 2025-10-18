module Main where

import           System.IO
import           Control.Exception 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Control.Monad
import           Utils.ReadCloser
import           Types.Utils.ReadCloser
import           Control.Concurrent.Chan
import           Control.Concurrent

main :: IO ()
main = do
  let filePath = "/Users/venkat.kumar/Documents/venkat.kumar/http-server/message.txt"
  eitherHandle <- tryOpenFile filePath
  case eitherHandle of
    Left   e -> putStr $ show e ++ "\r\n"
    Right  h -> do
      let rc = mkReadCloser h 
      ch <- getLinesChannel rc
      consume ch

getLinesChannel :: ReadCloser -> IO (Chan (Maybe BS.ByteString))
getLinesChannel rc = do
  ch <- newChan

  void $ 
    forkIO $
      bracket 
        (pure ())
        (\_ -> runCloser (rcCloser rc) >> writeChan ch Nothing)
        (\_ -> go ch BS.empty)
  
  return ch
  where   
    go ch ln = do
      chunk <- runReader (rcReader rc) 8
      if (BS.null chunk)
        then when (not (BS.null ln)) (writeChan ch (Just ln))
        else do
          let parts = BS.split 10 chunk
          case parts of
            [x] -> go ch (BS.append ln x)
            _   -> 
              writeChan ch (Just $ BS.append ln (head parts))
                >> mapM_ (\p -> writeChan ch (Just p)) (init (tail parts))
                  >> go ch (last parts)

consume :: Chan (Maybe BS.ByteString) -> IO ()
consume ch = 
  readChan ch >>= \l ->
    case l of
      Just l' -> BSC.putStrLn l' >> consume ch
      Nothing -> return ()

tryOpenFile :: FilePath -> IO (Either IOError Handle)
tryOpenFile filePath = try (openFile filePath ReadMode) :: IO (Either IOError Handle)