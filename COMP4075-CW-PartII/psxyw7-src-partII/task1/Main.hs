import System.Random
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

philosopherNames :: [String]
philosopherNames = ["Socrates", "Anthony", "Denny", "Edwin", "Leo"]

type ForkSemaphore = TVar Bool
type Message = TQueue String

emptySemaphore :: IO ForkSemaphore
emptySemaphore = newTVarIO True -- avaliable by default

useOneFork :: ForkSemaphore -> STM ()
useOneFork fork = do
    state <- readTVar fork
    if state then writeTVar fork False else retry

releaseOneFork :: ForkSemaphore -> STM ()
releaseOneFork = flip writeTVar True -- make one fork avaliable

delaySomeTime :: IO ()
delaySomeTime = do
    mus <- randomRIO (1000, 1200) -- microsecond
    threadDelay $ mus * 1000 -- millisecond

eat :: String -> Message -> ForkSemaphore -> ForkSemaphore -> IO ()
eat name msgs left right = do
    atomically $ writeTQueue msgs (name ++ " is hungry.")

    (leftNum, rightNum) <- atomically $ do
        leftNum <- useOneFork left
        rightNum <- useOneFork right
        return(leftNum, rightNum)
    atomically $ writeTQueue msgs (name ++ " got forks " ++ " and is now eating.")
    delaySomeTime -- start eating
    atomically $ writeTQueue msgs (name ++ " is done eating. Going back to thinking.")
    atomically $ do
        releaseOneFork left
        releaseOneFork right
    delaySomeTime -- start thinking

    eat name msgs left right -- repeat

run :: [String] -> IO ()
run names = do
    forks <- replicateM cnt emptySemaphore
    msgQueue <- newTQueueIO
    forM_ [0..cnt-1] (\i ->
        forkIO $ eat (names !! i) msgQueue (forks !! i) (forks !! (i + 1 `mod` cnt)))
    printMsg msgQueue
    return ()
    where
        cnt = length names
        printMsg :: Message -> IO String
        printMsg queue = do
                s <- atomically (readTQueue queue)
                putStrLn s
                printMsg queue

main :: IO ()
main = do
    run philosopherNames
