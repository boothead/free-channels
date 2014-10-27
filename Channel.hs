{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Channel where

-- import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Free
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Sequence (Seq, viewl, ViewL(..), (|>))
import qualified Data.Sequence as S

--------------------------------------------------------------------------------

type ChanState a = IORef (Seq a)

data Chan a = Chan {
    putChan :: a -> IO () 
  , getChan :: IO (Maybe a)
  }

newChan :: IO (Chan a)
newChan = do
  q <- newIORef S.empty
  return $ Chan (putQ q) (getQ q) -- (putQ _) (getQ q)

getQ :: ChanState a -> IO (Maybe a)
getQ q = do
  chan <- readIORef q
  case viewl chan of
    EmptyL -> return Nothing
    a :< as -> do
      writeIORef q as
      return $ Just a
  
putQ :: ChanState a -> a -> IO ()
putQ q a = do
  modifyIORef q (|> a)

--------------------------------------------------------------------------------

data ChannelF a next = Put (Chan a) a next
                     | Recv (Chan a) (Maybe a -> next)
                     | MkChan (Chan a -> next)
     deriving (Functor)
     
type Channel a m = FreeT (ChannelF a) m

putC :: MonadFree (ChannelF a) m => Chan a -> a -> m ()
putC chan a = liftF $ Put chan a ()

recvC :: MonadFree (ChannelF a) m => Chan a -> m (Maybe a)
recvC chan = liftF $ Recv chan id

mkChan :: MonadFree (ChannelF a) m => m (Chan a)
mkChan = liftF $ MkChan id


--------------------------------------------------------------------------------

prog :: (MonadIO m) => Channel Int m String
prog = do
  chan <- mkChan
  putC chan 1
  el <- recvC chan
  el2 <- recvC chan
  liftIO $ print (el, el2)
  forM_ [5..10] $ putC chan
  sequence_ $ replicate 3 $ do
     e <- recvC chan
     liftIO $ print e
  return $ show (el, el2)

--------------------------------------------------------------------------------

interpret :: MonadIO m => FreeT (ChannelF a) m b -> m b
interpret chan' = do
  c <- runFreeT chan'
  case c of
    Pure a -> return a
    
    (Free (MkChan nxt)) -> do
      interpret $ nxt =<< liftIO newChan
      
    (Free (Put chan a nxt)) -> do
      liftIO $ putChan chan a
      interpret nxt
    
    (Free (Recv chan nxt)) -> do
      interpret $ nxt =<< liftIO (getChan chan)
      
test :: IO String
test = interpret prog
