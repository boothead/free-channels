{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Channel where

import           Control.Applicative
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

data ChannelF next = forall a. Put (Chan a) a next
                   | forall a. Recv (Chan a) (Maybe a -> next)
                   | forall a. MkChan (Chan a -> next)

instance Functor ChannelF where
  fmap f (Put chan a next) = Put chan a (f next)
  fmap f (Recv chan next)  = Recv chan (f . next)
  fmap f (MkChan next)     = MkChan (f . next)
     
type Channel m = FreeT ChannelF m

putC :: MonadFree ChannelF m => Chan a -> a -> m ()
putC chan a = liftF $ Put chan a ()

recvC :: MonadFree ChannelF m => Chan a -> m (Maybe a)
recvC chan = liftF $ Recv chan id

mkChan :: MonadFree ChannelF m => m (Chan a)
mkChan = liftF $ MkChan id


--------------------------------------------------------------------------------

prog :: (MonadIO m) => Channel m (Maybe (Int, String))
prog = do
  chan <- mkChan
  chan2 <- mkChan
  putC chan (1 :: Int)
  el <- recvC chan
  el2 <- recvC chan
  liftIO $ print (el, el2)
  forM_ [5..10 :: Int] $ (putC chan2) . show
  sequence_ $ replicate 6 $ do
     e <- recvC chan2
     case e of
       Nothing -> return ()
       Just n -> liftIO $ putStrLn ("Chan2 got: " ++ n)
  putC chan 10
  putC chan2 "Bye"
  eInt <- recvC chan
  eStr <- recvC chan2
  return $ (,) <$> eInt <*> eStr

--------------------------------------------------------------------------------

interpret :: MonadIO m => FreeT ChannelF m b -> m b
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
      
test :: IO (Maybe (Int, String))
test = interpret prog
