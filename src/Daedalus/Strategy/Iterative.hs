{-# LANGUAGE TypeFamilies, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Daedalus.Strategy.Iterative (IterativeT, iterativeStrategy, runIterativeT) where

import Control.Monad.RWS
import Data.Monoid
import Daedalus.StrategySearch
import Daedalus.Cost

newtype IterativeT m t = IterativeT { unIterativeT :: RWST Int Any() m t }
	deriving (Functor, Applicative, Monad, MonadTrans)

instance (MonadReader r m) => MonadReader r(IterativeT m) where
	ask = IterativeT(lift ask)
instance (MonadWriter r m) => MonadWriter r(IterativeT m) where
	writer = IterativeT. lift.writer
instance (MonadState r m) => MonadState r(IterativeT m) where
	state = IterativeT. lift.state

iterativeStrategy :: (Modality m)
	=> IterativeT m t
	-> IterativeT m t
iterativeStrategy m = IterativeT(ask>>= \depth->
	if depth>0 then local pred(unIterativeT m)
		else tell(Any True)>>return(error"tech unused"))

instance (Costly m) => Costly(IterativeT m) where
	type CostOf (IterativeT m) = CostOf m
	cost c heuristic = lift(IterativeT ask)>>= \depth->
		expHoistStrategySearch lift
			(\m->liftM fst(evalRWST(unIterativeT m) depth ()))
			(cost c heuristic)
	getCost = lift(IterativeT ask)>>= \depth->
		expHoistStrategySearch lift
			(\m->liftM fst(evalRWST(unIterativeT m) depth ()))
			getCost

_runIterativeT :: (Modality m)
	=> IterativeT m t
	-> m t
_runIterativeT m = rec 1 where
	rec depth = evalRWST(unIterativeT m) depth ()>>= \(x,Any bool)->
		if bool then
			if depth == maxBound then
				fail"runIterativeT: reached max depth"
			else rec(succ depth)
			else return x

-- | 'runIterativeT'
--
-- Note that unlike other strategies this one will cause the procedure to be run multiple times.
runIterativeT :: (Modality m)
	=> StrategySearchT(IterativeT m) t
	-> StrategySearchT m t
runIterativeT = expHoistStrategySearch _runIterativeT lift.
	addStrategy(techExpHoistSearchT id iterativeStrategy)