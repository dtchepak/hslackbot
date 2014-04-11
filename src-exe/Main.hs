import HSlackBot
import System.Environment

main :: IO ()
main =
  let
    usage = do
      projName <- getProgName
      putStrLn $ "usage: " ++ projName ++ " [-sslKey (keyFile) -sslCert (certFile)] (portNumber)"
    tryStart s p =
        case reads p of
            [(portNum, [])] -> s portNum
            _               -> usage
  in do
      args <- getArgs
      case args of
        ["-sslKey", keyFile, "-sslCert", certFile, port]
                -> tryStart (startSSL keyFile certFile) port
        [port]  -> tryStart start port
        _       -> usage


