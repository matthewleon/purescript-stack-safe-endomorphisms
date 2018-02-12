module Data.Monoid.Endo.StackSafe 
  ( Endo
  , endo
  , applyEndo
  ) where

import Prelude

import Data.CatList (CatList)
import Data.Foldable (foldl)
import Data.Functor.Invariant (class Invariant)
import Data.Monoid (class Monoid)
import Data.Monoid.Endo as Explosive
import Data.Newtype (class Newtype, wrap)

-- TODO: ideally, we would have a nonempty CatList here
newtype Endo a = Endo (CatList (Explosive.Endo a))

derive instance newtypeEndo :: Newtype (Endo a) _

derive newtype instance semigroupEndo :: Semigroup (Endo a)

instance monoidEndo :: Monoid (Endo a) where
  mempty = endo id

instance invariantEndo :: Invariant Endo where
  imap ab ba es = endo (ab <<< applyEndo es <<< ba)

-- | Constructor
endo :: forall a. (a -> a) -> Endo a
endo = wrap <<< pure <<< wrap

applyEndo :: forall a. Endo a -> a -> a
applyEndo (Endo es) x = foldl go x es
  where
  go :: a -> Explosive.Endo a -> a
  go acc (Explosive.Endo f) = f acc
