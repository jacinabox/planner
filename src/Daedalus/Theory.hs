{-|
A partial semantics

* Left distributivity:

  @
  mzero >>= f = mzero
  m `mplus` m2 >>= f = (m>>=f) `mplus` (m2>>=f)
  @

* Right distributivity (presuming that the operation 'm' is idempotent):

  @
  m >> mzero = mzero
  m >>= \x->f x `mplus` f2 x = (m>>=f) `mplus` (m>>=f2)
  @

* Equations surrounding cut:

  @
  cut >>= f = cut
  cut `mplus` m = cut
  scope(m `mplus` cut) = scope m
  @

  (These equations are not sufficient to calculate fully with 'cut' and 'scope'. Calculating fully with
  'cut' and 'scope' requires a non-local calculation rule. This non-local rule is as follows:

  scope m = m', provided m does not contain 'scope', and with m' the result of replacing all appearances
  of 'cut' in m with 'mzero'.

  This rule allows to further reduce an expression containing scopes from the outside in.)

* Left- and right-distributivity of scope:

  @
  scope(m>>=f) = scope m>>=f
  scope(lift m>>=f) = lift m>>=scope.f
  @

  (The latter is only workable if 'm' is a lifted operation: for instance the calculation
  /scope cut = cut>>scope(pure())/ would not be valid, as it changes the scope in which
  the cut applies.)

* Well-behavedness of costs:

  @
  cost mempty minBound = pure()
  cost c heuristic >> cost c2 heuristic2 = cost(c<>c2) (heuristic `max` heuristic2)
  @

* Idempotence of strategies (which has to be maintained on a strategy-by-strategy basis):

  @
  applyStrategy(applyStrategy m) = applyStrategy m
  @

* Equations for 'addStrategy'

  @
  addStrategy f(addStrategy f2 m) = addStrategy(f.f2) m
  strategySearchT m>>=addStrategy f.g = addStrategy f(strategySearchT m>>=g)
  @

------------------------------------

A non-observable effect is governed by the following equation:

  @
  void m = pure()
  @

  (If you don't need the result, it doesn't matter whether you run it or not.)

  Examples include 'ask' and 'get', but many effects can be handled as empirically non-observable,
  such as reading a file or checking the current time.

  An idempotent effect has the same result if run twice. In order to define this properly
  I need the abstract data type UseT r m := ReaderT(m r) m, with the following sole operation:

  @
  use :: (Modality m) => UseT r m r
  use := ask>>=lift
  @

  and the following eliminator:

  @
  runUseT :: (Modality m) => UseT r m t->m r->m t
  runUseT = runReaderT.unUseT
  @

  Now I define idempotence as:
  m>>=uncurry runUseT.(f &&& const m) = m>>=uncurry runUseT.(f &&& pure)

  As a special case of this equation, liftM2(,) m m = liftM dup m
    with dup x = (x,x).-}

module Daedalus.Theory where