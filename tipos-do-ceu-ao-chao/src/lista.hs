import Prelude hiding (foldl)
import Control.Applicative (liftA2)

data List a = Nil | Cons a (List a)

list :: List a -> b -> (a -> List a -> b) -> b
list  Nil        b _ = b
list (Cons a as) _ f = f a as

psi :: (a1 -> b1 -> c) -> (a -> a1) -> (b -> b1) -> a -> b -> c
psi bin f g a b = bin (f a) (g b)

instance Semigroup (List a) where
    (<>) la lb = list la lb $ psi Cons id (<> lb)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap f as = list as Nil $ psi Cons f (fmap f)

instance Foldable List where
    -- foldMap f as = list as mempty $ psi mappend f (foldMap f)
    foldr f b as = list as b $ psi f id (foldr f b)

instance Traversable List where
    sequenceA fas = list fas (pure Nil)
        $ psi (liftA2 Cons) id sequenceA
    -- traverse f as = list as (pure Nil)
    --     $ psi (liftA2 Cons) f (traverse f)
    -- --  $ psi (\ fa fb -> fmap Cons fa <*> fb) f (traverse f)

instance Show a => Show (List a) where
    show as = show $ toList as

foldl :: (a -> b -> b) -> b -> List a -> b
foldl f b as = list as b (\ a la -> f a $ foldl f b la)

fromList :: [a] -> List a
fromList = foldr Cons Nil

toList :: List a -> [a]
toList = foldr (:) []
