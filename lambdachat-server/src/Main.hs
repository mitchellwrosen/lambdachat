module Main where

import qualified System.ZMQ4 as Zmq


main :: IO ()
main = do
  Zmq.withContext $ \context ->
    Zmq.withSocket context Zmq.Pull $ \pullSocket ->
      Zmq.withSocket context Zmq.Pub $ \pubSocket -> do
        Zmq.bind pullSocket "ipc://lambdachat-pull.sock"
        Zmq.bind pubSocket "ipc://lambdachat-pub.sock"

        forever $ do
          bytes <- Zmq.receive pullSocket
          print bytes
          Zmq.send pubSocket [] bytes
