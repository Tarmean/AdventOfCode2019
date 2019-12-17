{-# Language RankNTypes #-}
{-# Language StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Logic (MonadLogic (..), Search(..), unique, fromList, runSearchStrict, runSearch1) where
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Set as S
import Control.Applicative

-- natLogic :: (forall x. m x -> n x) -> (forall x. n x -> m x) -> LogicT m a -> LogicT n a
-- natLogic nat rev (LogicT f) = LogicT $ \cons zero -> nat $ f (\a s -> rev (cons a (nat s))) (rev zero)

unique :: (Monad m, Ord a) => a ->  Search a m ()
unique a = Search $ do
    s <- get
    when (S.member a s) $ empty
    modify (S.insert a)
newtype Search b m a = Search { unSearch :: LogicT (StateT (S.Set b) m) a}
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, MonadLogic)
instance (MonadState s m) => MonadState s (Search b m) where
  get = Search . lift . lift $ get
  put = Search . lift . lift . put
instance (MonadIO m) => MonadIO (Search b m) where
  liftIO = lift . liftIO
  
instance MonadTrans (Search b) where
    lift = Search . lift . lift

fromList :: Monad m => [a] -> Search b m a
fromList = foldr (interleave) empty . map pure

runSearchStrict :: Monad m => Search b m a -> m [a]
runSearchStrict (Search m) = evalStateT (observeAllT m) S.empty

runSearch1 :: Monad m => Search b m a -> m a
runSearch1 (Search m) = evalStateT (observeT m) S.empty
