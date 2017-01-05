module Kanren.Op where

import Prelude
import Control.Alt ((<|>))
import Data.List.Lazy (List(..), Step(..), step)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, fst, curry)
import Kanren.Goal (Goal, BS(BS))
import Kanren.State (unify, SC(SC))
import Kanren.Value (LogicValue(Empty, Pair, LVar), quote, class AsLogicValue)

fairAppend :: forall a. List a -> List a -> List a
fairAppend xs ys = List (go <$> unwrap xs)
  where
  go Nil = step ys
  go (Cons x xs') = Cons x (fairAppend ys xs')

pureT :: ∀ a b f. (Applicative f) ⇒ a → b → f (Tuple a b)
pureT = curry pure

pureTU :: ∀ a f. (Applicative f) ⇒ a → f (Tuple a Unit)
pureTU = (flip (curry pure)) unit



infixl 4 equals as ?==

equals :: ∀ a b. (AsLogicValue a, AsLogicValue b) ⇒ a → b → Goal
equals l r = BS \(SC s c) → case unify (quote l) (quote r) s of
  Nothing → mempty
  Just s' → pureTU (SC s' c)



fresh :: BS LogicValue
fresh = BS \(SC s c) → pureT (SC s (c + 1)) (LVar c)



infixl 3 disjo as ?||
disjo :: Goal → Goal → Goal
disjo (BS a) (BS b) = BS \sc → fairAppend (a sc) (b sc)



infixl 3 conjo as ?&&
conjo :: Goal → Goal → Goal
conjo (BS a) (BS b) = BS \sc → a sc >>= fst >>> b



appendo :: ∀ a b c. (AsLogicValue a, AsLogicValue b, AsLogicValue c) ⇒ a → b → c → Goal
appendo l r out = appendo' (quote l) (quote r) (quote out)
  where appendo' l r out = do
            l ?== Empty
            r ?== out
          <|> do
            a ← fresh
            d ← fresh
            res ← fresh
            Pair a d ?== l
            Pair a res ?== out
            appendo' d r res
