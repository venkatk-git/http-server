module Utils.Configs where

import qualified Network.Simple.TCP as TCP

tcpSocketHost :: TCP.HostPreference
tcpSocketHost = (TCP.Host "127.0.0.1")

tcpSocketPort :: TCP.ServiceName
tcpSocketPort = "42069" 

readerChunkSize :: Int
readerChunkSize = 8