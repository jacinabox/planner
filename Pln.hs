{-# LANGUAGE Trustworthy #-}

-- | The notion of linearized plan is simply a sequence of actions with associated STRIPS attributes.
--  A linearized plan has structural redundancies corresponding to unordered actions.
--  This planner exploits these structural redundancies to greatly speed up planning
--  for problems with few ordering constraints.
module Pln (Action(..), checkPlan, plan, test2) where

import Data.Monoid
import Data.List
import Data.Maybe
import Data.Default
import Control.Monad.Search
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad
import Control.Applicative
import Control.DeepSeq
-- import System.IO.Unsafe

breakEnd :: (t -> Bool) -> [t] -> ([t], [t])
breakEnd f ls = (reverse xs2, reverse xs) where (xs, xs2) = break f(reverse ls)

-------------------------------

data Action s t = Action{ pre :: ![s], add :: ![s], del :: ![s], token :: !t } deriving (Show, Read, Eq, Ord)

instance (Eq s, Default t) => Monoid(Action s t) where
	mempty = Action[] [] [] def
	mappend a a2 = Action
		(union(pre a2) (pre a\\add a2))
		(union(add a) (add a2\\del a))
		(union(del a) (del a2\\add a))
		(token a2)

instance (NFData s, NFData t) => NFData(Action s t) where
	rnf action = rnf(pre action)`seq`rnf(add action)`seq`rnf(del action)`seq`rnf(token action)

hasConflict :: (Eq s) => Action s t -> Action s t -> Bool
hasConflict a a2 = not$null$intersect(add a) (del a2)
	++intersect(del a) (add a2)
	++intersect(pre a) (del a2)
	++intersect(del a) (pre a2)

transformPostcondition :: (Eq s) => [s] -> [Action s t] -> [s]
transformPostcondition = foldl'(\p a -> union(p\\add a) (pre a))

cleaned postcondition = catMaybes.snd.mapAccumL(\p a -> let p' = union(p\\add a) (pre a) in
	(p', do { guard$null$intersect(del a) p; return a })) postcondition

-------------------------------

-- prnt x= unsafePerformIO(print x>>return x)

actionRevisionPenalty :: Sum Float
actionRevisionPenalty = 100

costDelayedSum :: [t] -> Search(Sum Float) t
costDelayedSum (x:xs) = return x `mplus` (cost actionRevisionPenalty 0>>costDelayedSum xs)
costDelayedSum [] = mzero

_plan :: (Show s, Show t, Ord s, Ord t, Default t)
	=> ([s] -> Search(Sum Float) (Action s t))
	-> [s]
	-> [s]
	-> [s]
	-> [(Sum Float, Action s t)]
	-> WriterT(Sum Float) (StateT[[[Action s t]]] (Search(Sum Float))) [Action s t]
_plan f precondition postcondition postcondition2 dutyList = if null$postcondition2\\precondition then do
	otherTracks <- get
	let st:stack = otherTracks
	put stack
	return$!concat st
	else do
	otherTracks <- get
	(c,x):xs <- lift$lift$costDelayedSum$init$tails dutyList
	cost c 0
	guard$null$intersect(del x) postcondition
	guard$not$null$intersect(add x) postcondition2
	let partitions = unzip.map(breakEnd(hasConflict x)) <$> otherTracks
	let conf = fst <$> partitions
	put$![]:(snd <$> partitions)
	let linearizedTracks = concat$concat conf
	let postcondition' = transformPostcondition postcondition(linearizedTracks++[x])
	let postcondition'' = pre x
	-- Descend to evaluate a new sub-DAG
	let dutyList' = runSearch$f postcondition''
	(ls, penalty) <- lift$runWriterT$_plan f precondition postcondition' postcondition'' dutyList'
	cost(negate penalty) 0 -- Undo the penalty
	-- Proceed to evaluate other components
	let finalTrack = x:ls
	modify$ \(ls:lss) -> (finalTrack:ls):lss
	let postcondition''' = transformPostcondition postcondition2 linearizedTracks\\add x
	-- It determines whether or not to add another DAG layer by whether induced dependencies were found.
	-- If they were not found, it proceeds with the remaining duty list so that actions are considered
	-- strictly in order of generation by 'f'.
	let dutyList' = if null linearizedTracks then xs else runSearch$f postcondition'''
	liftM(linearizedTracks++)$_plan f precondition(transformPostcondition postcondition linearizedTracks\\add x) postcondition''' dutyList'

checkPlan :: (Eq s) => [Action s t] -> [s] -> [s] -> Bool
checkPlan plan precondition postcondition =
	let
	(preRequired, bool) = foldl'(\(p, bool) a -> (union(p\\add a) (pre a), bool&&null(intersect p(del a)))) (postcondition, True) plan in
	bool && null(preRequired\\precondition)

plan :: (Show s, Show t, Ord s, Ord t, Default t) => ([s] -> Search(Sum Float) (Action s t)) -> [s] -> [s] -> Search(Sum Float) [Action s t]
plan f pre post = evalStateT(liftM fst$runWriterT$do
	let dutyList = runSearch$f post
	ls <- _plan f pre post post dutyList
	otherTracks <- get
	let finalPlan = cleaned post$concat(concat otherTracks) ++ ls
	-- guard$checkPlan finalPlan pre post
	return finalPlan)
	[[]]

--------------------------------

cost'' :: Float -> Float -> Search(Sum Float) ()
cost'' x x2 = cost(Sum x) (Sum x2)

openTrunk s = case msum$map(stripPrefix "trunkopen") s of
	Just n -> cost'' 1 0>>return(Action((if n=="1" then ["trunkclosed2","trunkclosed3"] else [])++["trunkclosed"++n]) ["trunkopen"++n] ["trunkclosed"++n] ("openTrunk"++n))
	Nothing -> mzero

closeTrunk s = case msum$map(stripPrefix "trunkclosed") s of
	Just n -> cost'' 1 0>>return(Action ["trunkopen"++n] ["trunkclosed"++n] ["trunkopen"++n] ("closeTrunk"++n))
	Nothing -> mzero

unscrewTire s = case msum$map(stripPrefix "tireoff") s of
	Just n -> cost'' 1 0>>return(Action ["tireon"++n] ["tireoff"++n] ["tireon"++n] ("unscrewTire"++n))
	Nothing -> mzero

addTire s = case msum$map(stripPrefix "newtireon") s of
	Just n -> cost'' 1 0>>return(Action ["tireoff"++n,"newtireout"++n] ["newtireon"++n] ["newtireout"++n] ("addTire"++n))
	Nothing -> mzero

takeOutTire s = case msum$map(stripPrefix "newtireout") s of
	Just n -> cost'' 1 0>>return(Action ["trunkopen"++n] ["newtireout"++n] [] ("takeOutTire"++n))
	Nothing -> mzero

putAwayTire s = case msum$map(stripPrefix "putaway") s of
	Just n -> cost'' 1 0>>return(Action ["trunkopen"++n,"tireoff"++n] ["putaway"++n] [] ("putAwayTire"++n))
	Nothing -> mzero

acts2 s = msum[openTrunk s, closeTrunk s, unscrewTire s, addTire s, takeOutTire s, putAwayTire s]

test2 = plan acts2 --["trunkclosed1", "tireon1"] ["trunkclosed1", "putaway1", "newtireon1"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5", "trunkclosed6", "tireon6", "trunkclosed7", "tireon7"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5", "trunkclosed6", "newtireon6", "putaway6", "trunkclosed7", "newtireon7", "putaway7"]
	["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5", "trunkclosed6", "tireon6", "trunkclosed7", "tireon7", "trunkclosed8", "tireon8", "trunkclosed9", "tireon9", "trunkclosed10", "tireon10"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5", "trunkclosed6", "newtireon6", "putaway6", "trunkclosed7", "newtireon7", "putaway7", "trunkclosed8", "newtireon8", "putaway8", "trunkclosed9", "newtireon9", "putaway9", "trunkclosed10", "newtireon10", "putaway10"]
