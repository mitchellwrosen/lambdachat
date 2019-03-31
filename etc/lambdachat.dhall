let
  Peer =
    { name : Text
    , identity : Text
    }
in
  {
    identity = None Text,
    peers = [] : List Peer
  }
