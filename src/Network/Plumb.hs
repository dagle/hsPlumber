module Network.Plumb (
    ) where

--import Data.NineP
import System.Environment
import Text.Printf
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe
import System.IO
import Control.Monad

data Attr = Attr {
    name    :: String
    , value   :: String
}

data Message = Message {
    src     :: String
    , dst     :: String
    , dir     :: String
    , typ     :: String
    , attr    :: [Attr]
    , dat     :: [Word8]
}

{-
getEnv' env = do
    str <- try $ getEnv env
    case str of 
        (Left _) -> return ""
        (Right s) -> return s

namespace = do
    ns <- getEnv' "NAMESPACE"
    if ns /= ""
        then
            ns
        else
            d <- getEnv' "DISPLAY"
            disp <- getDisplay d
            user <- getEnv' "USER" 
            return $ printf "/tmp/ns.%s.%s" user disp

dial str = do 
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix str)
    send sock $ runPut (put $ Msg TTversion (-1) $ Tversion 1024 "9P2000") 
    -- should parse the response
    _ <- recv sock 50
    return sock

nofid = 0xffffffff

plumbConnect = do 
    ns <- namespace
    sock <- dial (ns </> "plumb")
    user <- getUser
    fid <- newFid
    send sock $ runPut (put $ Msg TTattach (-1) $ Tattach nofid fid "" user)
    _ <- recv sock 50
    return (fid, sock)

plumbOpen sock fid name filemode = do
    send sock $ runPut (put $ Msg TTwalk (-1) $ Twalk fid nfid [])
    m <- liftM (runGet get) $ recv sock 50
    9pOpen name filemode
    return fid

plumbWrite fid message =
plumbRead fid =
-}

consume :: ForeignPtr Word8 -> Int -> Int -> IO [Word8]
consume fp ix len
  | ix == len = return []
  | otherwise = do
     c <- withForeignPtr fp $ \p -> peekElemOff p ix
     cs <- unsafeInterleaveIO (consume fp (ix+1) len)
     return (c : cs)

produce :: ForeignPtr Word8 -> Int -> Int -> [Word8] -> IO ()
produce fp ix len w
  | ix == len = return ()
  | otherwise = do
     c <- withForeignPtr fp $ \p -> pokeElemOff p ix (w !! ix)
     cs <- unsafeInterleaveIO (produce fp (ix+1) len w)
     return ()

parseAttr str =
    let pairs = words str
        nvPair = map (span ((==) '=')) pairs
        in map (\(n, v) -> Attr n v) nvPair

reciveMsg :: Handle -> IO Message
reciveMsg h = do
    src <- hGetLine h
    dst <- hGetLine h
    dir <- hGetLine h
    typ <- hGetLine h
    attr <- liftM parseAttr $ hGetLine h
    num <- liftM read $ hGetLine h
    fp <- mallocForeignPtrBytes num
    len <- withForeignPtr fp $ \buf -> hGetBuf h buf num
    buf <- consume fp 0 len
    return $ Message src dst dir typ attr buf

sendMsg :: Handle -> Message -> IO ()
sendMsg h msg = do
    hPutStrLn h (src msg)
    hPutStrLn h (dst msg)
    hPutStrLn h (dir msg)
    hPutStrLn h (typ msg)
    hPutStrLn h (foldr (\(Attr n v) b ->  n ++ "=" ++ v ++ " " ++ b ) "" $ attr msg)
    let len = length $ dat msg
    hPutStrLn h (show len)
    fp <- mallocForeignPtrBytes len
    produce fp 0 len (dat msg)
    withForeignPtr fp $ \buf -> hPutBuf h buf len
