{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, TupleSections, ScopedTypeVariables, ConstraintKinds, StandaloneDeriving, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

module Daedalus.Strategy.AStar (AStarState(..), heuristicEstimate, AStarT, aStarStrategy, runAStarT) where

import Data.PQueue.Prio.Min
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative hiding (empty)
import Prelude hiding (map, null)
import qualified Prelude
-- import System.IO.Unsafe
import Daedalus.Cost
import Daedalus.SearchT

data AStarState c = AStarState { costSoFar :: !c, recentHeuristic :: !c } deriving Show

-- | The search tree has to be partially reified to achieve the effect of a AStar search.
--   You generally do find something algebraic; sometimes it's a bit strange.
newtype AStarT c m t = AStarT { unAStarT :: WriterT
	(MinPQueue c(SearchT(AStarT c m) ()))
	(StateT
	(AStarState c)
	m)
	t } deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadReader r m, Monoid c, Ord c) => MonadReader r(AStarT c m)
instance (MonadWriter r m, Monoid c, Ord c) => MonadWriter r(AStarT c m) where
	writer = AStarT. lift.writer
instance (MonadState r m, Monoid c, Ord c) => MonadState r(AStarT c m) where
	state = AStarT. lift.lift.state

instance (Ord c) => MonadTrans(AStarT c) where
	{-# INLINE lift #-}
	lift = AStarT. lift.lift

{-instance (Ord c) => MFunctor(AStarT c) where
	hoist f = AStarT. hoist(hoist f).unAStarT-}

-------------------------------------------

{-# INLINE heuristicEstimate #-}
heuristicEstimate state = costSoFar state <> recentHeuristic state

aStarStrategy :: (Modality m, Monoid c, Ord c)
	=> SearchT(AStarT c m) t
	-> SearchT(AStarT c m) t
-- | The strategy to use on disjunctions just captures the parameter and puts it in a priority
--   queue for later.
aStarStrategy m = lift(AStarT(get>>= \cost->
	let mResumption = lift(AStarT(put cost))>>void m in
	tell(singleton(heuristicEstimate cost) mResumption)>>
	return(error"aStarStrategy: tech unused")))

instance (Modality m, Monoid c, Ord c, Bounded c) => Costly(AStarT c m) where
	type CostOf(AStarT c m) = c
	{-# INLINE _cost #-}
	_cost c heuristic =
		lift(AStarT get)>>= \state->
		techCoda
			(lift(AStarT(put state)))
			(lift(AStarT(put$!AStarState(costSoFar state <> c) heuristic)))
		>>techResolveCallback aStarStrategy(return())
	{-# INLINE _getCost #-}
	_getCost = liftM(\state->(costSoFar state,recentHeuristic state)) (lift(AStarT get))

-------------------------------------------

-- | 'runAStarT' operator does much of the work for the A* strategy.
--
-- Example:
--
-- >>> runAStarT$((cost(5::Sum Int) 0>>return 'A')<|>(cost 11 0>>return 'B')<|>(cost 3 0>>return 'C'))>>=lift.lift.print
-- 'C'
-- 'A'
-- 'B'
--
runAStarT :: forall m c. (Modality m, Monoid c, Ord c, Bounded c)
	=> SearchT(AStarT c m) ()
	-> m()
runAStarT =
	(`evalStateT` AStarState mempty minBound).
	rec.
	(>>= \x->cost mempty mempty>>return x) -- Cause strategy to be triggered.
	where

	rec :: SearchT(AStarT c m) () -> StateT(AStarState c) m()
	rec m = do
		priority <- (execWriterT.
			unAStarT.
			void.
			runSearchT) m
		-- The actions from the priority queue have to be "glued back together" in ascending
		-- cost order. They are run back through 'rec' (so long as there are still elements to process).
		maybe
			(return())
			(\(m',priority')->rec(m'<|>lift(AStarT(tell priority'))))
			(minView priority)
