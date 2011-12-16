
-- {{{ 10.1
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']
-- }}}
-- {{{ 10.3
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data Tree = Leaf Int | Node Tree Int Tree

occures :: Int -> Tree -> Bool
occures m (Leaf n) = m == n
occures m (Node l n r)
 | m == n = True
 | m < n = occures m l
 | otherwise = occures m r

-- }}}
-- {{{ 10.4
-- {{{ from 9
rmdups :: Eq a => [a] -> [a]
-- rmdups = nub
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
-- }}}

data Prop = Const Bool
 | Var Char
 | Not Prop
 | And Prop Prop
 | Imply Prop Prop

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
--bools 0 = []
--bools n = map (False:) bss ++ map (True:) bss
-- where
--  bss = bools (n - 1)
bools n = sequence eles
 where
  eles = take n $ repeat [True, False]

substs :: Prop -> [Subst]
substs p = map (zip vs) $ bools $ length vs
 where
  vs = rmdups $ vars p

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- }}}

