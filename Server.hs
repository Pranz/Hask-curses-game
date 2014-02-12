
import Network
import System.IO

main = withSocketsDo $ 
    do
        socket <- listenOn . portNumber $ 2048
        sequence . repeat $ acceptConnection socket
    where acceptConnection :: Socket ->  IO ()
          acceptConnection socket = do
              
    