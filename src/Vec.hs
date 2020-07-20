module Vec where

vquot :: Integral b => (b, b) -> b -> (b, b)
(x, y) `vquot` n = (x `quot` n, y `quot` n)

vneg :: (Num a, Num b) => (a, b) -> (a, b)
vneg (x, y) = (-x, -y)

vsignum :: (Num a, Num b) => (a, b) -> (a, b)
vsignum (x, y) = (signum x, signum y)

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

(<->) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
p <-> q = p <+> vneg q

infixl 6 <+>, <->
infixl 7 `vquot`
