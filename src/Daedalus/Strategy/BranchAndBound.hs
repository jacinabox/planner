{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}

module Daedalus.Strategy.BranchAndBound (BranchT, branchAndBoundStrategy, branchAndBoundCallback, runBranchT) where

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.IO.Class
import Control.Applicative
import Daedalus.SearchT
import Daedalus.Cost

-- In production the debug messages go into this stub.
unsafePerformIO _ = ()

-------------------------------------------

data BranchAndBound c = BranchAndBound { costSoFar :: !c, upperBound :: !c } deriving Show

newtype BranchT c m t = BranchT { unBranchT :: StateT(BranchAndBound c) m t }
	-- It derives these things.
	deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans, MonadIO)

instance (Modality m, Ord c, Monoid c, Bounded c) => Costly(BranchT c m) where
	type CostOf(BranchT c m) = c
	{-# INLINE _cost #-}
	_cost c heuristic = do
		branchAndBoundStrategy(return())
		branchState <- lift(BranchT get)
		let c' = costSoFar branchState <> c
		-- When total cost estimate exceeds the upper bound, this branch will be aborted.
		-- 'heuristic' is not stored in the state, simply
		-- as it is not used except in this procedure to do cuts.
		when(c' <> heuristic >= upperBound branchState) cut
		lift(BranchT(put$!branchState { costSoFar = c' }))
	_getCost = liftM(\state->(costSoFar state,error"BranchAndBound: heuristic un-represented")) (lift(BranchT get))

instance MFunctor(BranchT c) where
	hoist f = BranchT. hoist f.unBranchT

deriving instance (MonadReader r m, Monoid c, Ord c) => MonadReader r(BranchT c m)
deriving instance (MonadWriter r m, Monoid c, Ord c) => MonadWriter r(BranchT c m)
instance (MonadState r m, Monoid c, Ord c) => MonadState r(BranchT c m) where
	state = BranchT. lift.state

branchAndBoundStrategy :: (Modality m, Ord c, Monoid c)
	=> SearchT(BranchT c m) t
	-> SearchT(BranchT c m) t
branchAndBoundStrategy m = lift(BranchT get)>>= \boundState->techCoda
	(
	-- Updates the upper bound with the new information.
	lift(BranchT(modify'(\boundState2->
		boundState { upperBound = upperBound boundState2 }))))
	(scope>>m)

branchAndBoundCallback :: (Modality m, Ord c) => BranchT c m()
branchAndBoundCallback = BranchT(modify'(\boundState->boundState { upperBound = min(upperBound boundState) (costSoFar boundState) }))

-----------------------------------

-- | An elimination form that runs the branch and bound strategy.
runBranchT :: (Modality m, Monoid c, Ord c, Bounded c)
	=> SearchT(BranchT c m) t
	-> m(Maybe t)
-- The callback has to be shimmed to include some more bookkeeping.
runBranchT = (`evalStateT` BranchAndBound mempty maxBound).
	unBranchT.
	runSearchT.
	(>>= \x->lift branchAndBoundCallback>>return x)