{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Daedalus.Cost (Costly(..)) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as S
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans
import Daedalus.SearchT
import Daedalus.StrategySearch
import System.IO.Unsafe
import Data.IORef

-- | A generic concept of cost functions and heuristic for modalities, adapted from the function of the same
-- name in monad-dijkstra package.
class (Modality m) => Costly m where
	type CostOf m :: *
	cost :: CostOf m -> CostOf m -> StrategySearchT m()
	getCost :: StrategySearchT m(CostOf m,CostOf m)

-----------------------------------------------

-- A degenerate case for 'cost' has no costs recorded.
instance Costly Identity where
	type CostOf Identity = ()
	{-# INLINE cost #-}
	cost _ _ = return()
	{-# INLINE getCost #-}
	getCost = return$!((),())

cheesyWriterHoist :: (Monoid r, Modality m) => StrategySearchT m t
	-> StrategySearchT(WriterT r m) t
cheesyWriterHoist n = expHoistStrategySearch lift
	(\m->runWriterT m>>= \(x,w)->return$!unsafePerformIO(writeIORef ref w>>return x))
	n>>= \x->
	lift(tell$!unsafePerformIO$readIORef ref)>>
	return x
	where
	{-# NOINLINE ref #-}
	ref = unsafePerformIO(newIORef mempty)

-- Instances that pass costs through.
instance (Monoid r, Costly m) => Costly(WriterT r m) where
	type CostOf(WriterT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = cheesyWriterHoist.cost c
	{-# INLINE getCost #-}
	getCost = cheesyWriterHoist getCost

instance (Costly m) => Costly(ReaderT r m) where
	type CostOf(ReaderT r m) = CostOf m
	{-# INLINE cost #-}
	cost c h = lift ask>>= \r->
		expHoistStrategySearch lift(`runReaderT` r) (cost c h)
	getCost = lift ask>>= \r->
		expHoistStrategySearch lift(`runReaderT` r) getCost

instance (MonadWriter r m) => MonadWriter r(SearchT m) where
	writer = lift.writer
	-- TODO implement the remaining 'writer' methods.
instance (MonadReader r m) => MonadReader r(SearchT m) where
	ask = lift ask

instance (MonadWriter r m) => MonadWriter r(StrategySearchT m) where
	writer = lift.writer
instance (MonadReader r m) => MonadReader r(StrategySearchT m) where
	ask = lift ask
{-
instance (Costly m) => Costly(StateT r m) where
	type CostOf(StateT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
	getCost = hoist lift getCost
instance (Costly m) => Costly(S.StateT r m) where
	type CostOf(S.StateT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
	getCost = hoist lift getCost

instance (Costly m) => Costly(ErrorT r m) where
	type CostOf(ErrorT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
	getCost = hoist lift getCost-}

-----------------------------------------------
-- Some orphan instances

instance Bounded Float where
	minBound = -1/0
	maxBound = 1/0

instance Bounded Double where
	minBound = -1/0
	maxBound = 1/0

deriving instance (Fractional f) => Fractional(Sum f)