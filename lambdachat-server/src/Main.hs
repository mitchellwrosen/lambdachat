import Control.Concurrent (forkIO)

import qualified System.ZMQ4 as Zmq

main :: IO ()
main = do
  Zmq.withContext $ \context ->
    Zmq.withSocket context Zmq.Pull $ \pullSocket ->
      Zmq.withSocket context Zmq.Pub $ \pubSocket -> do
        Zmq.bind pullSocket "ipc://lambdachat-pull.sock"
        Zmq.bind pubSocket "ipc://lambdachat-pub.sock"

        forever $ do
          msg <- Zmq.receive pullSocket
          Zmq.send pubSocket [] msg
