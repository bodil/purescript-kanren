module Kanren where

import Prelude
import Data.Array as Array
import Data.List as List
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (pureST, writeSTRef, readSTRef, ST, newSTRef, STRef)
import Control.Monad.Trans (lift, class MonadTrans)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(Just, Nothing))



newtype StreamT m a = StreamT (∀ r. (a → m (List.List r)) → m (List.List r))

runStreamT :: ∀ r m a. StreamT m a → (a → m (List.List r)) → m (List.List r)
runStreamT (StreamT f) k = f k

instance functorStreamT :: (Monad m) ⇒ Functor (StreamT m) where
  map f m = StreamT (\k → runStreamT m (\a → k $ f a))

instance applyStreamT :: (Monad m) ⇒ Apply (StreamT m) where
  apply f v = StreamT (\k → runStreamT f $ (\g → runStreamT v (\a → (k $ g a))))

instance bindStreamT :: (Monad m) ⇒ Bind (StreamT m) where
  bind m k = StreamT (\k' → runStreamT m (\a → runStreamT (k a) k'))

instance applicativeStreamT :: (Monad m) ⇒ Applicative (StreamT m) where
  pure a = StreamT (\k → k a)

instance monadStreamT :: (Monad m) ⇒ Monad (StreamT m)



instance streamMonadTrans :: MonadTrans StreamT where
  lift m = StreamT \k → m >>= k

lower :: ∀ m a. (Monad m) ⇒ StreamT m a → m (List.List a)
lower (StreamT f) = f (pure >>> pure) -- one for the list, one for the monad m

type Kanren s a = StreamT (Eff (st :: ST s)) a

success :: ∀ s. Kanren s Unit
success = lift $ pure unit

failure :: ∀ s a. Kanren s a
failure = StreamT \k -> pure List.Nil



type LRef = STRef

newLRef :: ∀ s a. a → Kanren s (LRef s a)
newLRef = newSTRef >>> lift

readLRef :: ∀ s a. LRef s a → Kanren s a
readLRef = readSTRef >>> lift

writeLRef :: ∀ s a. LRef s a → a → Kanren s Unit
writeLRef ref a = StreamT \k → do
  a' ← readSTRef ref -- read old value
  writeSTRef ref a    -- write new value
  r ← k unit         -- perform search
  writeSTRef ref a'   -- restore the old value
  pure r              -- return result of search



type Var s a = LRef s (Maybe a)

-- freshVar :: ∀ s a b. (Var s a → b) → Kanren s b
-- freshVar f = liftM1 f $ newLRef Nothing

data Atom s = VarA (Var s (Atom s))
            | Atom String

data List s a = VarL (Var s (List s a))
              | Nil
              | Cons a (List s a)

infixr 6 Cons as :

class Fresh s a where -- | a → s
  fresh :: Kanren s a

instance freshAtom :: Fresh s (Atom s) where
  fresh = VarA `liftM1` newLRef Nothing

instance freshList :: Fresh s (List s a) where
  fresh = VarL `liftM1` newLRef Nothing



instance showAtom :: Show (Atom s) where
  show (Atom a) = a
  show (VarA v) = "?atom"



isProperList :: ∀ s a. List s a → Boolean
isProperList (Cons car cdr) = isProperList cdr
isProperList Nil = true
isProperList _ = false

toArray :: ∀ s a. List s a → Maybe (Array a)
toArray l@(Cons _ _) | isProperList l =
  Just $ List.toUnfoldable $ convert l
  where convert (Cons car cdr) = List.Cons car $ convert cdr
        convert _ = List.Nil
toArray _ = Nothing

instance showList :: (Show a) ⇒ Show (List s a) where
  show (VarL v) = "?list"
  show p@(Cons _ _) | isProperList p = case toArray p of
    Just l → show l
    Nothing → "bad list"
  show (Cons car cdr) = "(" <> show car <> " . " <> show cdr <> ")"
  show Nil = "[]"

fromFoldable :: ∀ s a f. (Foldable f) ⇒ f a → List s a
fromFoldable = foldr (\car cdr → Cons car cdr) Nil



class Unify s a where -- | a → s
  isVar :: a → Maybe (Var s a)
  unify :: a → a → Kanren s Unit

unifyVar :: ∀ s a. (Unify s a) ⇒ Var s a → a → Kanren s Unit
unifyVar ref a = do
  mb ← readLRef ref
  case mb of
    Nothing → writeLRef ref (Just a)
    Just b → a ?== b

infixl 4 equals as ?==

equals :: ∀ s a. (Unify s a) ⇒ a → a → Kanren s Unit
equals a b = case isVar a, isVar b of
  Just v1, Just v2 | v1 == v2 → success
  Just v, _ → unifyVar v b
  _, Just v → unifyVar v a
  _, _ → unify a b



instance unifyAtom :: Unify s (Atom s) where
  isVar (VarA var) = Just var
  isVar _ = Nothing

  unify (Atom s1) (Atom s2) | s1 == s2 = success
  unify _ _ = failure



run :: ∀ a. (∀ s. Kanren s a) → List.List a
run m = pureST (lower m)

variable :: ∀ a b s. (a → Kanren s b) → Var s a → Kanren s b
variable convert var = do
  ma ← readLRef var
  case ma of
    Just a → convert a
    Nothing → failure

atom :: ∀ s. Atom s → Kanren s String
atom (Atom s) = pure s
atom (VarA var) = variable atom var

list :: ∀ a b s. (a → Kanren s b) → List s a → Kanren s (Array b)
list _ Nil = pure []
list elt (Cons a as) = lift2 (Array.cons) (elt a) (list elt as)
list elt (VarL var) = variable (list elt) var



-- If PS had type classes with functional dependencies, this would type check.
foo = run do
  a ← fresh
  equals a (Atom "b")
