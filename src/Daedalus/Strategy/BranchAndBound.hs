{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}

module Daedalus.Strategy.BranchAndBound (BranchT, branchAndBoundStrategy, branchAndBoundCallback, runBranchT) where

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Applicative
import Daedalus.StrategySearch
import Daedalus.SearchT
import Daedalus.Cost

-- In production the debug messages go into this stub.
unsafePerformIO _ = ()

-------------------------------------------

data BranchAndBound c = BranchAndBound { costSoFar :: !c, upperBound :: !c } deriving Show

newtype BranchT c m t = BranchT { unBranchT :: StateT(BranchAndBound c) m t }
	-- It derives these things.
	deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans)

instance (Modality m, Ord c, Monoid c) => Costly(BranchT c m) where
	type CostOf(BranchT c m) = c
	{-# INLINE cost #-}
	cost c heuristic = do
		branchState <- lift(BranchT get)
		let c' = costSoFar branchState <> c
		-- When total cost estimate exceeds the upper bound, this branch will be aborted.
		-- 'heuristic' is not stored in the state, simply
		-- as it is not used except in this procedure to do cuts.
		when(c' <> heuristic >= upperBound branchState)$strategySearchT cut
		strategySearchT$techCoda
			(\x->
			-- Updates the upper bound with the new information.
			lift(BranchT(modify'(\branchState2->branchState { upperBound = upperBound branchState2 })))>>
			return x)
			-- Store the updated cost.
			(lift(BranchT(put$!branchState { costSoFar = c' })))
	getCost = liftM(\state->(costSoFar state,error"BranchAndBound: heuristic un-represented")) (lift(BranchT get))

deriving instance (MonadReader r m, Monoid c, Ord c) => MonadReader r(BranchT c m)
deriving instance (MonadWriter r m, Monoid c, Ord c) => MonadWriter r(BranchT c m)
instance (MonadState r m, Monoid c, Ord c) => MonadState r(BranchT c m) where
	state = BranchT. lift.state

branchAndBoundStrategy :: (Modality m, Ord c, Monoid c)
	=> SearchT(BranchT c m) t
	-> SearchT(BranchT c m) t
branchAndBoundStrategy = scope
{-	 = lift(BranchT get)>>= \boundState->techCoda
		(\x->
		-- Updates the upper bound with the new information.
		lift(BranchT(modify'(\boundState2->unsafePerformIO(print$"reset at"++show(boundState { upperBound = upperBound boundState2 }))`seq`
			boundState { upperBound = upperBound boundState2 })))>>
		return x)
		(scope m)-}

branchAndBoundCallback :: (Modality m, Ord c) => BranchT c m()
branchAndBoundCallback = BranchT(modify'(\boundState->boundState { upperBound = min(upperBound boundState) (costSoFar boundState) }))

-----------------------------------

-- | An elimination form that runs the branch and bound strategy.
runBranchT :: (Modality m, Monoid c, Ord c, Bounded c)
	=> StrategySearchT(BranchT c m) ()
	-> m()
-- The callback has to be shimmed to include some more bookkeeping.
runBranchT = (`evalStateT` BranchAndBound mempty maxBound).
	unBranchT.
	runSearchT.
	runStrategySearchT.
	addStrategy branchAndBoundStrategy.
	(>>lift branchAndBoundCallback)
