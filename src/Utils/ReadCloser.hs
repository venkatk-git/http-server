module Utils.ReadCloser where

import           Types.Utils.ReadCloser
import qualified Network.Socket as SocketT
import qualified Network.Simple.TCP as TCP


mkReadCloser :: SocketT.Socket -> ReadCloser
mkReadCloser s = 
  ReadCloser 
    { rcReader = Reader (\n -> TCP.recv s n)
    , rcCloser = Closer (TCP.closeSock s)
    }