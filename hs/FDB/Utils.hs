module FDB.Utils where

compose2 f g x y = f (g x y)

f .: g = f `compose2` g

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
