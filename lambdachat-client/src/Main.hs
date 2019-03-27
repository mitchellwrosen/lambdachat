import Control.Concurrent (forkIO)

import qualified Data.ByteString.Char8 as Latin1
import qualified System.ZMQ4           as Zmq

main :: IO ()
main =
  Zmq.withContext $ \context ->
    Zmq.withSocket context Zmq.Push $ \pushSocket ->
      Zmq.withSocket context Zmq.Sub $ \subSocket -> do
        Zmq.connect pushSocket "ipc://lambdachat-pull.sock"
        Zmq.connect subSocket "ipc://lambdachat-pub.sock"
        Zmq.subscribe subSocket ""

        forkIO $ forever $ do
          Zmq.receive subSocket >>= print

        forever $ do
          line <- Latin1.getLine
          Zmq.send pushSocket [] line
