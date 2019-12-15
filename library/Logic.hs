{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Logic (MonadLogic (..), Search(..), unique, fromList) where
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
instance MonadTrans (Search b) where
    lift = Search . lift . lift

fromList :: Monad m => [a] -> Search b m a
fromList = foldr (<|>) empty . map pure



