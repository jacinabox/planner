{-# LANGUAGE ConstraintKinds, Rank2Types, DeriveFunctor, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Daedalus.SearchT (Modality, SearchT(..), scope, cut, techResolveCallback, techExpHoistCodensity, techExpHoistSearchT, techCoda, runSearchT, collectSearchT) where

import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Codensity
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Applicative

type Modality = Monad

_codensityMonadPlus (Codensity f) (Codensity f2) = Codensity(\x -> f x>>f2 x)

newtype SearchT m t = SearchT { unSearchT :: Codensity(MaybeT m) t }
	-- The definition of alternative comes from the /outer/ continuation modality transformer.
	deriving (Functor, Monad, Applicative)

instance MonadTrans SearchT where
	{-# INLINE lift #-}
	lift = SearchT. lift.lift

instance (Monad m) => MonadPlus(SearchT m) where
	mzero = SearchT mzero
	mplus (SearchT f) (SearchT f2) = SearchT(_codensityMonadPlus f f2)

instance (Monad m) => Alternative(SearchT m) where
	empty = mzero
	(<|>) = mplus

-- | 'scope' and 'cut' work similarly as the 'seal' and 'collapse' functions respectively from monad-dijkstra.
--   'cut' works similarly as Prolog's cut operation but uses 'scope' blocks to control how far the cut
--   extends.
{-# INLINE scope #-}
scope :: (Modality m)
	=> SearchT m()
scope = techExpHoistSearchT(liftM(const undefined).lift.runMaybeT) id (return())

{-# INLINE cut #-}
cut :: (Modality m)
	=> SearchT m a
cut = SearchT(lift mzero)

-- | 'techResolveCallback' is a tool to make sure that polymorphic transformations operate
--   over enough of the program, so that left-distributivity can be satisfied.
techResolveCallback :: forall m t. (Modality m)
	=> (forall tech. SearchT m tech->SearchT m tech)
	-> SearchT m t
	-> SearchT m t
techResolveCallback f (SearchT(Codensity f2)) = SearchT(Codensity(\f3->
	runCodensity
	(unSearchT$f$SearchT$lift$f2 f3)
	return))

{-# INLINE techExpHoistCodensity #-}
techExpHoistCodensity :: (forall r. m r->n r)
	-> (forall r. n r->m r)
	-> Codensity m t
	-> Codensity n t
techExpHoistCodensity f f2 (Codensity f3) = Codensity(\f4->f(f3(f2.f4)))

-- | 'SearchT' enjoys exponential maps in the category of functors.
{-# INLINE techExpHoistSearchT #-}
techExpHoistSearchT :: (forall t. MaybeT m t->MaybeT n t)
	-> (forall t. MaybeT n t->MaybeT m t)
	-> SearchT m t
	-> SearchT n t
techExpHoistSearchT f f2= SearchT. techExpHoistCodensity f f2. unSearchT

-- | 'techCoda' applies a function to run following the operation chronologically. It can be
-- used to do cleanup on a branch.
{-# INLINE techCoda #-}
techCoda :: (Modality m)
	=> MaybeT m()
	-> SearchT m t
	-> SearchT m t
techCoda f (SearchT(Codensity f2)) = SearchT(Codensity(\f3->f2 f3>>= \r->f>>return r))

-------------------------------------
-- Eliminators at the SearchT type constructor.

-- | A basic eliminator for 'SearchT' modality transformer.
runSearchT :: (Modality m)
	=> SearchT m t
	-> m(Maybe t)
runSearchT (SearchT m) = runMaybeT$runCodensity m return

-- | Example:
--
-- >>> collectSearchT(scope(return(1::Int)<|>cut<|>return(2::Int))<|> return 3)
-- [1,3]
collectSearchT :: (Modality m)
	=> SearchT(WriterT[t] m) t
	-> m[t]
collectSearchT = execWriterT
	.runSearchT
	.(>>=lift.tell.return)
