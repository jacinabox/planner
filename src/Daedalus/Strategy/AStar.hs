{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveFunctor, TupleSections, ScopedTypeVariables, ConstraintKinds #-}

module Daedalus.Strategy.AStar (AStarState(..), heuristicEstimate, AStarT, aStarStrategy', runAStarT) where

import Data.PQueue.Prio.Min
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad
import Control.Applicative hiding (empty)
import Prelude hiding (map, null)
import qualified Prelude
-- import System.IO.Unsafe
import Daedalus.Cost
import Daedalus.SearchT
import Daedalus.StrategySearch

data AStarState c = AStarState { costSoFar :: !c, recentHeuristic :: !c } deriving Show

-- | The search tree has to be partially reified to achieve the effect of a AStar search.
--   You generally do find something algebraic; sometimes it's a bit strange.
newtype AStarT c m t = AStarT { unAStarT :: WriterT
	(MinPQueue c(SearchT(AStarT c m) ()))
	(StateT
	(AStarState c)
	m)
	t } deriving (Functor, Applicative, Monad)

instance (Ord c) => MonadTrans(AStarT c) where
	{-# INLINE lift #-}
	lift = AStarT. lift.lift

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

aStarStrategy' :: (Modality m, Monoid c, Ord c)
	=> SearchT(AStarT c m) t
	-> SearchT(AStarT c m) t
aStarStrategy' = techResolveCallback aStarStrategy

instance (Modality m, Monoid c, Ord c) => Costly(AStarT c m) where
	type CostOf(AStarT c m) = c
	{-# INLINE cost #-}
	cost c heuristic = lift(AStarT get)>>= \state->
		strategySearchT$techCoda
			(\x -> lift(AStarT(put state))>>return x)
			(lift(AStarT(put$!AStarState(costSoFar state <> c) heuristic)))
--		>>aStarStrategy'(return())

-------------------------------------------

-- | 'runAStarT' operator does much of the work for the A* strategy.
--
-- Example:
--
-- >>> runAStarT$((cost(5::Sum Int) 0>>applyStrategy(return 'A'))<||>((cost 11 0>>applyStrategy(return 'B'))<||>(cost 3 0>>applyStrategy(return 'C'))))>>=lift.lift.print
-- 'C'
-- 'A'
-- 'B'
--
-- Nota bene that with this strategy it is important to apply 'applyStrategy' in the leaves of the
-- user code computation.
runAStarT :: forall m c. (Modality m, Monoid c, Ord c, Bounded c)
	=> StrategySearchT(AStarT c m) ()
	-> m()
runAStarT =
	(`evalStateT` AStarState mempty minBound).
	rec.
	runStrategySearchT
	.addStrategy aStarStrategy' where

	rec :: SearchT(AStarT c m) () -> StateT(AStarState c) m()
	rec m = do
		priority <- (execWriterT.
			unAStarT.
			runSearchT) m
		-- The actions from the priority queue have to be "glued back together" in ascending
		-- cost order. They are run back through 'rec' (so long as there are still elements to process).
		maybe
			(return())
			(\(m',priority')->rec(m'<|>lift(AStarT(tell priority'))))
			(minView priority)
