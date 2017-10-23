{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, ScopedTypeVariables #-}

module Daedalus.SearchT (Modality, SearchT(..), techCallcc2, scope, cut, techResolveCallback, techExpHoistContT, techExpHoistSearchT, techCoda, runSearchT, collectSearchT) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Applicative
import Unsafe.Coerce

type Modality = Monad

instance (Modality m, Monoid r) => MonadPlus(ContT r m) where
	{-# INLINE mzero #-}
	mzero = ContT(\_ -> return mempty)
	{-# INLINE mplus #-}
	mplus m m2 = ContT(\f -> runContT m f>>runContT m2 f)

instance (Modality m, Monoid r) => Alternative(ContT r m) where
	{-# INLINE empty #-}
	empty = mzero
	{-# INLINE (<|>) #-}
	(<|>) = mplus

newtype SearchT m t = SearchT { unSearchT :: forall tech. (Monoid tech) => ReaderT(ContT tech m tech) (ContT tech(ContT tech m)) t }
	-- The definition of alternative comes from the /outer/ continuation modality transformer.
	deriving Functor

instance (Modality m) => Monad(SearchT m) where
	{-# INLINE return #-}
	return x = SearchT(return x)
	{-# INLINE (>>=) #-}
	m >>= f = SearchT(unSearchT m>>=unSearchT. f)
instance (Modality m) => Applicative(SearchT m) where
	{-# INLINE pure #-}
	pure = return
	{-# INLINE (<*>) #-}
	(<*>) = ap
instance (Modality m) => MonadPlus(SearchT m) where
	{-# INLINE mzero #-}
	mzero = SearchT mzero
	{-# INLINE mplus #-}
	mplus m m2 = SearchT(mplus(unSearchT m) (unSearchT m2))
instance (Modality m) => Alternative(SearchT m) where
	{-# INLINE empty #-}
	empty = mzero
	{-# INLINE (<|>) #-}
	(<|>) = mplus

instance MonadTrans SearchT where
	{-# INLINE lift #-}
	lift m = SearchT(lift(lift(lift m)))

{-# INLINE techCallcc2 #-}
techCallcc2 f = ask>>= \x->lift(ContT(\k->callCC((`runContT` k).(`runReaderT` x).f)))

-- | 'scope' and 'cut' work similarly as the 'seal' and 'collapse' functions respectively from monad-dijkstra.
--   'cut' works similarly as Prolog's cut operation but uses 'scope' blocks to control how far the cut
--   extends.
{-# INLINE scope #-}
scope :: (Modality m)
	=> SearchT m t
	-> SearchT m t
scope m = SearchT(techCallcc2(\cont->local(const(cont(error"tech unused"))) (unSearchT m)))

{-# INLINE cut #-}
cut :: (Modality m)
	=> SearchT m a
cut = SearchT(ask>>=lift.lift>>return(error"no return"))

{-# INLINE techCoerce #-}
techCoerce :: ContT tech m t
	-> ContT tech2 m t
techCoerce = unsafeCoerce

{-# INLINE techCoerce2 #-}
techCoerce2 :: ContT tech m tech
	-> ContT tech2 m tech2
techCoerce2 = unsafeCoerce

{-# INLINE techCoerce3 #-}
techCoerce3 :: ReaderT(ContT tech m tech) (ContT tech(ContT tech m)) t
	-> ReaderT(ContT tech m tech) (ContT tech(ContT tech2 m)) t
techCoerce3 = unsafeCoerce

-- | 'techResolveCallback' is a tool to make sure that polymorphic transformations operate
--   over enough of the program, so that left-distributivity can be satisfied.
{-# INLINE techResolveCallback #-}
techResolveCallback :: forall m t. (Modality m)
	=> (forall tech. SearchT m tech->SearchT m tech)
	-> SearchT m t
	-> SearchT m t
techResolveCallback f m = SearchT(ask>>= \cont->
	lift(ContT(\cont2->
		let m' = SearchT(lift(lift(runContT
			(runReaderT(techCoerce3(unSearchT m)) (techCoerce2 cont))
			(techCoerce.cont2)))) in
		runContT(runReaderT(unSearchT(f m')) cont) return)))

{-# INLINE techExpHoistContT #-}
techExpHoistContT :: (m r->n r2)
	-> (n r2->m r)
	-> ContT r m t
	-> ContT r2 n t
techExpHoistContT f f2 m = ContT(\g->f(runContT m(f2.g)))

-- | 'SearchT' enjoys exponential maps in the category of functors.
{-# INLINE techExpHoistSearchT #-}
techExpHoistSearchT :: (forall t. m t->n t)
	-> (forall t. n t->m t)
	-> SearchT m t
	-> SearchT n t
techExpHoistSearchT f f2 m = SearchT(ask>>= \cont->
	lift$techExpHoistContT(techExpHoistContT f f2) (techExpHoistContT f2 f) (runReaderT
		(unSearchT m)
		(techExpHoistContT f2 f cont)))

-- | 'techCoda' applies a function to run following the operation chronologically. It can be
-- used to do cleanup on a branch.
{-# INLINE techCoda #-}
techCoda :: (forall tech. (Monoid tech) => tech->ContT tech m tech)
	-> SearchT m t
	-> SearchT m t
techCoda f m =
	SearchT$ask>>= \cont2->lift(ContT(\cont->runContT(runReaderT(unSearchT m) cont2) cont>>=f))

-------------------------------------
-- Eliminators at the SearchT type constructor.

-- | A basic eliminator for 'SearchT' modality transformer.
runSearchT :: (Modality m)
	=> SearchT m()
	-> m()
runSearchT m = runContT(callCC$ \cont->(`runContT` return).(`runReaderT` cont(error"tech unused")).unSearchT$m) return

-- | Example:
--
-- >>> collectSearchT(scope(return(1::Int)<|>cut<|>return(2::Int))<|> return 3)
-- [1,3]
collectSearchT :: (Modality m)
	=> SearchT(WriterT[t] m) t
	-> m[t]
collectSearchT = execWriterT.runSearchT.(>>=lift.tell.return)
