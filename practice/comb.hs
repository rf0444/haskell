import Control.Applicative
--import Control.Monad

s :: (a -> b -> c) -> (a -> b) -> a -> c
s = (<*>)

k :: a -> b -> a
k = pure

i :: a -> a
i = s k k

f :: a -> b -> b
f = k i

b :: (b -> c) -> (a -> b) -> a -> c
--b = fmap
b = s (k s) k

c :: (a -> b -> c) -> b -> a -> c
c = s (b b s) (k k)

w :: (a -> a -> b) -> a -> b
--w = join
w = s s (k i)


--m = s i i
--l = c b m
--l = b w b
--y = s l l

