module Types.Utils.ReadCloser where

import Data.ByteString

newtype Reader = Reader { runReader :: Int -> IO (Maybe ByteString) }
newtype Closer = Closer { runCloser :: IO () }

data ReadCloser = 
  ReadCloser
    { rcReader  :: Reader 
    , rcCloser  :: Closer
    }