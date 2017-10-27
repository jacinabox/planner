{-# LANGUAGE Trustworthy, TypeFamilies, FlexibleContexts #-}

-- | The notion of linearized plan is simply a sequence of actions with associated STRIPS attributes.
--  A linearized plan has structural redundancies corresponding to unordered actions.
--  This planner exploits these structural redundancies to greatly speed up planning
--  for problems with few ordering constraints.
module Daedalus.Plan (Action(..), UserLevelStrategy, checkPlan, plan, test, test2) where

import Data.Monoid
import Data.List
import Data.Maybe
import Data.Default
import Data.Function
import Daedalus.Cost
import Daedalus.StrategySearch
import Daedalus.Strategy.AStar
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad
import Control.Applicative
import Control.DeepSeq
import System.IO.Unsafe

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

type UserLevelStrategy cost s t = StrategySearchT(AStarT cost(Writer[(Action s t, (cost, cost))])) (Action s t)

hasConflict :: (Eq s) => Action s t -> Action s t -> Bool
hasConflict a a2 = not$null$intersect(add a) (del a2)
	++intersect(del a) (add a2)
	++intersect(del a) (pre a2)
	++intersect(pre a) (del a2)

transformPostcondition :: (Eq s, Default t) => [s] -> [Action s t] -> [s]
transformPostcondition post ls = union(post\\add a) (pre a) where
	a = mconcat ls

transformPostconditionForward :: (Eq s, Default t) => [Action s t] -> [s]
transformPostconditionForward ls = add a\\del a where
	a = mconcat ls

transformPostconditionForward2 ls = foldl' union[] (transformPostconditionForward <$> ls)

cleaned postcondition = catMaybes.snd.mapAccumL(\p a -> let p' = union(p\\add a) (pre a) in
	(p', do { guard$null$intersect(del a) p; return a })) postcondition

-------------------------------

-- prnt x= unsafePerformIO(print x>>return x)

actionRevisionPenalty :: (Fractional f) => f
actionRevisionPenalty = 2

costDelayedSum :: (Costly m, Fractional(CostOf m), Monoid(CostOf m)) => [t] -> StrategySearchT m t
costDelayedSum (x:xs) = return x <||> (cost actionRevisionPenalty 0>>costDelayedSum xs)
costDelayedSum [] = mzero

findDutyList m =
	execWriter
	$runAStarT
	$liftM2(,) m getCost
	>>=tell.return

_plan :: (Show s, Show t, Ord s, Ord t, Default t, Costly m, Fractional(CostOf m), Bounded(CostOf m), Ord(CostOf m), Monoid(CostOf m))
	=> ([s] -> UserLevelStrategy(CostOf m) s t)
	-> [s]
	-> [s]
	-> [s]
	-> [(Action s t, (CostOf m, CostOf m))]
	-> WriterT(CostOf m) (StateT[[[Action s t]]] (StrategySearchT m)) [Action s t]
_plan f precondition postcondition postcondition2 dutyList = if null$postcondition2\\precondition then do
	otherTracks <- get
	let st:stack = otherTracks
	put stack
	-- unsafePerformIO(print(concat st))`seq`return()
	return$!concat st
	else do
	otherTracks <- get
	((x,cState):xs, i) <- lift$lift$foldl'(<||>) mzero$
		map return$
		zip(init$tails dutyList) [0::Int ..]
	-- unsafePerformIO(print x)`seq`return()
	lift$lift$uncurry cost cState
	let pen = fromIntegral i*actionRevisionPenalty
	lift$lift$cost pen 0
	tell pen
	guard$null$intersect(del x) postcondition
	guard$not$null$intersect(add x) postcondition2
	bool <- lift$lift$return True <||> return False
	-- Required to guess the ordering in case of a conflict.
	(linearizedTracks,linearizedTracks2) <- if bool then do
		let partitions = unzip.map(breakEnd(hasConflict x)) <$> otherTracks
		let conf = fst <$> partitions
		let linearizedTracks = concat$concat conf
		put$![]:(snd <$> partitions)
		return(linearizedTracks,[])
		else do
		let partitions = unzip.map(break(hasConflict x)) <$> otherTracks
		let conf = snd <$> partitions
		put$![]:(fst <$> partitions)
		let linearizedTracks2 = concat$concat conf
		guard$not$null linearizedTracks2
		return([],linearizedTracks2)
	-- unsafePerformIO(print(token <$> linearizedTracks,token <$> linearizedTracks2))`seq`return()
	let postcondition' = transformPostcondition postcondition(linearizedTracks++[x])
	let dutyList' = findDutyList$f(pre x)
	let aheadOfOrderConflict = any(not.null.intersect(pre x).del.fst) dutyList'
	let postcondition'' = if aheadOfOrderConflict then
		postcondition'
		else
		pre x
	-- Descend to evaluate a new sub-DAG
	(ls, penalty) <- lift$runWriterT$_plan f(union precondition(transformPostconditionForward linearizedTracks2)) postcondition' postcondition'' dutyList'
	-- guard$checkPlan ls precondition postcondition''
	lift$lift$cost(negate penalty) 0 -- Undo the penalty
	-- Proceed to evaluate other components
	let finalTrack = x:ls
	modify$ \(ls:lss) -> (finalTrack:ls):lss
	-- The single primed variant of 'postcondition' tracks the set of invariants that have to be maintained
	-- to avoid an in-order dependency. The double primed variant tracks only the tokens required to be
	-- satisfied in depth-first fashion. While on return this program guarantees to have solved the
	-- double primed postconditions, it is useful and necessary to track what postconditions it happened
	-- to satisfy in addition, and remove them from the postcondition sets.
	tracks:_ <- get
	let postcondition''' = transformPostcondition postcondition2 linearizedTracks\\transformPostconditionForward2 tracks
	-- It determines whether or not to add another DAG layer by whether induced dependencies were found.
	-- If they were not found, it proceeds with the remaining duty list so that actions are considered
	-- strictly in order of generation by 'f'.
	let dutyList' = if null linearizedTracks then
			xs
		else
			findDutyList$f postcondition'''
	liftM((++linearizedTracks2).(linearizedTracks++))$_plan f precondition(transformPostcondition postcondition linearizedTracks\\transformPostconditionForward2 tracks) postcondition''' dutyList'

checkPlan :: (Eq s) => [Action s t] -> [s] -> [s] -> Bool
checkPlan plan precondition postcondition =
	let
	(preRequired, bool) = foldl'(\(p, bool) a -> (union(p\\add a) (pre a), bool&&null(intersect p(del a)))) (postcondition, True) plan in
	bool && null(preRequired\\precondition)

plan :: (Show s, Show t, Ord s, Ord t, Default t, Costly m, Fractional(CostOf m), Bounded(CostOf m), Ord(CostOf m), Monoid(CostOf m))
	=> ([s] -> UserLevelStrategy(CostOf m) s t)
	-> [s]
	-> [s]
	-> StrategySearchT m[Action s t]
plan f pre post = evalStateT(liftM fst$runWriterT$do
	let dutyList = findDutyList$f post
	ls <- _plan f pre post post dutyList
	otherTracks <- get
	let finalPlan = cleaned post$concat(concat otherTracks) ++ ls
	-- guard$checkPlan finalPlan pre post
	return finalPlan)
	[[]]

--------------------------------

cost'' :: (Costly m, CostOf m ~ Sum Float) => Float -> Float -> StrategySearchT m ()
cost'' x x2 = cost(Sum x) (Sum x2)

heuristic s =
	let
	x = case msum$map(stripPrefix "x") s of
		Just n->fromIntegral(read n::Int)
		Nothing->10
	y = case msum$map(stripPrefix "y") s of
		Just n->fromIntegral(read n::Int)
		Nothing->10 in
	cost'' 1(abs(x-3)+abs(y-3))

succ' = show.(succ::Int->Int).read
pred' = show.(pred::Int->Int).read
guard' n = guard(m>=1&&m<=5) where
	m = read n::Int

goLeft s = case msum(map(stripPrefix "x") s) <|> msum(map(stripPrefix "formerlyx") s) of
	Just n-> guard' n>>heuristic s>>return(Action['x':succ' n] ['x':n] ['x':succ' n] "goleft")
	Nothing -> mzero

goRight s = case msum(map(stripPrefix "x") s)  <|> msum(map(stripPrefix "formerlyx") s) of
	Just n-> guard' n>>heuristic s>>return(Action['x':pred' n] ['x':n] ['x':pred' n] "goright")
	Nothing -> mzero

goUp s = case msum(map(stripPrefix "y") s)  <|> msum(map(stripPrefix "formerlyy") s) of
	Just n-> guard' n>>heuristic s>>return(Action['y':succ' n] ['y':n] ['y':succ' n] "goup")
	Nothing -> mzero

goDown s = case msum(map(stripPrefix "y") s)  <|> msum(map(stripPrefix "formerlyy") s) of
	Just n-> guard' n>>heuristic s>>return(Action['y':pred' n] ['y':n] ['y':pred' n] "godown")
	Nothing -> mzero

takeKey s = if "havekey" `elem` s then
		heuristic s>>return(Action["x4","y4"] ["havekey"] [] "takekey")
	else
		mzero

goToExit s = if "exit" `elem` s then
		cost''(-5) 0>>return(Action["x5","y5","havekey"] ["exit"] [] "gotoexit")
	else
		mzero

acts s = msum[takeKey s,goToExit s,goLeft s,goRight s,goUp s,goDown s]

test :: (Costly m, CostOf m ~ Sum Float) => StrategySearchT m[Action String String]
test = plan acts ["x1","y1"] ["exit"]-- >>= \acts->
{-	guard(token(last acts) == "start")>>
	guard(all((/="start").token) (init acts))>>
	return acts-}

--------------------------------

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

test2 :: (Costly m, CostOf m ~ Sum Float) => StrategySearchT m[Action String String]
test2 = plan acts2 --["trunkclosed1", "tireon1"] ["trunkclosed1", "putaway1", "newtireon1"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5"]
	-- ["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5", "trunkclosed6", "tireon6", "trunkclosed7", "tireon7"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5", "trunkclosed6", "newtireon6", "putaway6", "trunkclosed7", "newtireon7", "putaway7"]
	["trunkclosed1", "tireon1", "trunkclosed2", "tireon2", "trunkclosed3", "tireon3", "trunkclosed4", "tireon4", "trunkclosed5", "tireon5", "trunkclosed6", "tireon6", "trunkclosed7", "tireon7", "trunkclosed8", "tireon8", "trunkclosed9", "tireon9", "trunkclosed10", "tireon10"] ["trunkclosed1", "newtireon1", "putaway1", "trunkclosed2", "newtireon2", "putaway2", "trunkclosed3", "newtireon3", "putaway3", "trunkclosed4", "newtireon4", "putaway4", "trunkclosed5", "newtireon5", "putaway5", "trunkclosed6", "newtireon6", "putaway6", "trunkclosed7", "newtireon7", "putaway7", "trunkclosed8", "newtireon8", "putaway8", "trunkclosed9", "newtireon9", "putaway9", "trunkclosed10", "newtireon10", "putaway10"]

