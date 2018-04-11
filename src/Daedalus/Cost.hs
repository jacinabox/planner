{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Daedalus.Cost (Costly(..), cost, getCost) where
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.State as L
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Morph
import Daedalus.SearchT
import Daedalus.StrategySearch
import Data.IORef
import Data.Monoid

-- | A generic concept of cost functions and heuristic for modalities, adapted from the function of the same
-- name in monad-dijkstra package.
class (Modality m, Monoid(CostOf m), Bounded(CostOf m)) => Costly m where
	type CostOf m :: *
	_cost :: CostOf m -> CostOf m -> SearchT m()
	_getCost :: SearchT m(CostOf m,CostOf m)

-----------------------------------------------

instance (Monoid w, Monad m) => Costly(WriterT w m) where
	type CostOf(WriterT w m) = ()
	_cost _ _ = return()
	_getCost = return((),())

instance (Costly m) => Costly(ReaderT r m) where
	type CostOf(ReaderT r m) = CostOf m
	{-# INLINE _cost #-}
	_cost c h = lift ask>>= \r->
		techExpHoistSearchT lift(`runReaderT` r) (_cost c h)
	{-# INLINE _getCost #-}
	_getCost = lift ask>>= \r->
		techExpHoistSearchT lift(`runReaderT` r) _getCost

instance (Costly m) => Costly(L.StateT s m) where
	type CostOf(L.StateT s m) = CostOf m
	{-# INLINE _cost #-}
	_cost c h = lift get>>= \s->
		techExpHoistSearchT lift(`L.evalStateT` s) (_cost c h)
	{-# INLINE _getCost #-}
	_getCost = lift get>>= \s->
		techExpHoistSearchT lift(`L.evalStateT` s) _getCost

instance (Costly m) => Costly(StateT s m) where
	type CostOf(StateT s m) = CostOf m
	{-# INLINE _cost #-}
	_cost c h = lift get>>= \s->
		techExpHoistSearchT lift(`evalStateT` s) (_cost c h)
	{-# INLINE _getCost #-}
	_getCost = lift get>>= \s->
		techExpHoistSearchT lift(`evalStateT` s) _getCost

instance (MonadWriter r m) => MonadWriter r(SearchT m) where
	writer = lift.writer
	-- TODO implement the remaining 'writer' methods.
instance (MonadReader r m) => MonadReader r(SearchT m) where
	ask = lift ask

cost c = strategySearchT._cost c

getCost :: (Costly m) => StrategySearchT m(CostOf m,CostOf m)
getCost = strategySearchT _getCost

-----------------------------------------------
-- Some orphan instances

instance Bounded Float where
	minBound = -1/0
	maxBound = 1/0

instance Bounded Double where
	minBound = -1/0
	maxBound = 1/0

deriving instance (Fractional f) => Fractional(Sum f)
