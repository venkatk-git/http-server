module Utils.ReadCloser where

import           Data.ByteString 
import           Data.ByteString.Char8 
import           Control.Monad
import           System.IO
import           Types.Utils.ReadCloser


mkReadCloser :: Handle -> ReadCloser
mkReadCloser h = 
  ReadCloser 
    { rcReader = Reader (\n -> hGetSome h n)
    , rcCloser = Closer (hClose h)
    }