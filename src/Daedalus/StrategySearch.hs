{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Daedalus.StrategySearch (StrategySearchT, strategySearchT, applyStrategy, (<||>), expHoistStrategySearch, addStrategy, runStrategySearchT, module Daedalus.SearchT) where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Applicative
import Data.Monoid
import Daedalus.SearchT

newtype Strategy m = Strategy { unStrategy :: forall u. SearchT m u->SearchT m u }

instance Monoid(Strategy m) where
	{-# INLINE mempty #-}
	mempty = Strategy id
	{-# INLINE mappend #-}
	mappend s s2 = Strategy(unStrategy s.unStrategy s2)

------------------------------------

-- | 'StrategySearchT' allows to insert a (polymorphic) strategy combinator at every choice point
-- in a search.
newtype StrategySearchT  m t = StrategySearchT { unStrategySearchT :: ReaderT(Strategy m) (SearchT m) t } deriving
	(Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans StrategySearchT where
	{-# INLINE lift #-}
	lift = StrategySearchT. lift.lift

-- | It lifts a regular old 'SearchT', necessarily without applying any strategies within it.
{-# INLINE strategySearchT #-}
strategySearchT :: (Modality m)
	=> SearchT m t
	-> StrategySearchT m t
strategySearchT = StrategySearchT. lift

{-# INLINE applyStrategy #-}
applyStrategy :: (Modality m)
	=> StrategySearchT m t
	-> StrategySearchT m t
applyStrategy m = StrategySearchT(ask>>= \strategy->hoist
	(unStrategy strategy)
	(unStrategySearchT m))

infixl 3 <||>

-- | Convenience combinator for applying a strategy to both arms of a disjunction.
{-# INLINE (<||>) #-}
m <||> m2 = applyStrategy m <|> applyStrategy m2

{-# INLINE expHoistStrategySearch #-}
expHoistStrategySearch :: (Modality m, Modality n)
	=> (forall t. m t->n t)
	-> (forall t. n t->m t)
	-> StrategySearchT m t
	-> StrategySearchT n t
expHoistStrategySearch f f2 m = StrategySearchT$ask>>= \s->
	lift(techExpHoistSearchT f f2(runReaderT
		(unStrategySearchT m)
		(Strategy(techExpHoistSearchT f2 f.unStrategy s.techExpHoistSearchT f f2))))

------------------------------------

-- | Composes a strategy into the strategy chain.
addStrategy :: (Modality m)
	=> (forall u. SearchT m u->SearchT m u)
	-> StrategySearchT m t
	-> StrategySearchT m t
addStrategy f = StrategySearchT. local(Strategy f<>).unStrategySearchT

-- | A basic eliminator into the 'SearchT' modality transformer.
runStrategySearchT :: (Modality m)
	=> StrategySearchT m t
	-> SearchT m t
runStrategySearchT m = runReaderT(unStrategySearchT m) mempty