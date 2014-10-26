{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Channel where

import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.ST
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State (runStateT)
import           Data.Foldable (forM_)
import           Data.STRef
import           Data.Sequence (Seq, viewl, ViewL(..), (|>))
import qualified Data.Sequence as S

data ChannelF a next = Put (Chan a) a next
                     | Recv (Chan a) (Maybe a -> next)
                     | MkChan (Chan a -> next)
     deriving (Functor)

type Channel a m = FreeT (ChannelF a) m


newtype Chan a = Chan { unChan :: Seq a } deriving (Show)

putChan :: a -> Chan a -> Chan a
putChan a (Chan chan) = Chan $ chan |> a

takeChan :: Chan a -> Maybe (a, Chan a)
takeChan (Chan chan) = case viewl chan of
  EmptyL -> Nothing
  a :< as -> Just (a, Chan as)
  
newChan :: Chan a
newChan = Chan S.empty



putC :: MonadFree (ChannelF a) m => Chan a -> a -> m ()
putC chan a = liftF $ Put chan a ()

recvC :: MonadFree (ChannelF a) m => Chan a -> m (Maybe a)
recvC chan = liftF $ Recv chan id

mkChan :: MonadFree (ChannelF a) m => m (Chan a)
mkChan = liftF $ MkChan id


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


interpret :: (MonadState (Chan a) ms) => Channel a ms b -> ms b
interpret chan = do
  c <- runFreeT chan
  case c of
    Pure a -> return a
    
    (Free (MkChan nxt)) -> do
      let c = newChan
      put c
      interpret $ nxt newChan
      
    (Free (Put chan a nxt)) -> do
      modify $ putChan a
      interpret nxt
    
    (Free (Recv _ nxt)) -> do
      chan <- get
      case takeChan chan of
        Nothing -> interpret $ nxt Nothing
        Just (el, chan') -> do
          put chan'
          interpret $ nxt (Just el)
      
--test :: (MonadIO m, MonadFree (ChannelF Int) m) => m (String, Chan Integer)
--test :: (Monad m) => m (String, Chan a)
--test = flip runStateT (Chan S.empty) $ interpret prog
