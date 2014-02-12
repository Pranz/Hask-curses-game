
import Network
import System.IO

main = withSocketsDo $ do
    handle <- connectTo "localhost" (portNumber 2048)
    mapM (\msg ->
          hPutStr handle (msg ++ "\n") >>
          hFlush handle) $ lines input
    hClose handle
