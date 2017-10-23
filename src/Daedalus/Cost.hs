{-# LANGUAGE TypeFamilies #-}

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

-- | A generic concept of cost functions and heuristic for modalities, adapted from the function of the same
-- name in monad-dijkstra package.
class (Modality m) => Costly m where
	type CostOf m :: *
	cost :: CostOf m -> CostOf m -> StrategySearchT m()

-----------------------------------------------

-- A degenerate case for 'cost' has no costs recorded.
instance Costly Identity where
	type CostOf Identity = ()
	{-# INLINE cost #-}
	cost _ _ = return()

{-
-- Instances that pass costs through.
instance (Costly m) => Costly(WriterT r m) where
	type CostOf(WriterT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
instance (Costly m) => Costly(ReaderT r m) where
	type CostOf(ReaderT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
instance (Costly m) => Costly(StateT r m) where
	type CostOf(StateT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
instance (Costly m) => Costly(S.StateT r m) where
	type CostOf(S.StateT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c
instance (Costly m) => Costly(ErrorT r m) where
	type CostOf(ErrorT r m) = CostOf m
	{-# INLINE cost #-}
	cost c = hoist lift.cost c-}