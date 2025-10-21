module TCPListener.Main where

import           System.IO
import           Control.Exception 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Control.Monad
import           Utils.ReadCloser
import           Types.Utils.ReadCloser
import           Control.Concurrent.Chan
import           Control.Concurrent
import qualified Network.Simple.TCP as TCP
import qualified Utils.Configs as Configs

main :: IO ()
main = do
  (socket, socketAddr) <- TCP.bindSock Configs.tcpSocketHost Configs.tcpSocketPort
  putStrLn $ "Socket now accepts clients at: " ++ show socketAddr
  TCP.listenSock socket 2048
  forever $
    TCP.acceptFork socket $ \(clientSocket, _) -> do
      let rc = mkReadCloser clientSocket
      ch <- getLinesChannel rc
      consume ch
 
getLinesChannel :: ReadCloser -> IO (Chan (Maybe BS.ByteString))
getLinesChannel rc = do
  ch <- newChan

  void $ 
    forkIO $
      bracket 
        (pure ())
        (\_ -> 
          runCloser (rcCloser rc) >> 
            writeChan ch Nothing)
        (\_ -> loop ch BS.empty)
  
  return ch
  where   
    loop ch ln = do
      maybeChunk <- runReader (rcReader rc) Configs.readerChunkSize
      case maybeChunk of
        Just chunk -> do
          let parts = BS.split 10 chunk
          case parts of
            [x] -> loop ch (BS.append ln x)
            _   -> 
              writeChan ch (Just $ BS.append ln (head parts)) >> loop ch (last parts)
        Nothing    -> when (not (BS.null ln)) (writeChan ch (Just ln))

consume :: Chan (Maybe BS.ByteString) -> IO ()
consume ch = forever $ do
  readChan ch >>= \l ->
    case l of
      Just l' -> BSC.putStrLn l'
      Nothing -> return ()

tryOpenFile :: FilePath -> IO (Either IOError Handle)
tryOpenFile filePath = try (openFile filePath ReadMode) :: IO (Either IOError Handle)
