-- takes a function, lower bound, upper bound, and intervals, returns a number
trap :: (Num a, Fractional a, Enum a) => (a -> a) -> a -> a -> a -> a


trap f a b n =
    h * s
    where h = (b - a)/n                       -- size of interval
          f1 = f a                            -- first endpoint
          f2 = f b                            -- last endpoint
          t = (f1 + f2)/2                     -- 1 trapezoid
          height k = f $ a + k * h            -- height function for sample points
          s = foldl (+) t $ map height [1..(n-1)] -- sum everything up

-- left riemann sum, same args as trap
rmnnlft :: (Num a, Fractional a, Enum a) => (a -> a) -> a -> a -> a -> a

rmnnlft f a b n =
    h * s
    where h = (b-a)/n                          -- width of interval
          height k = f $ a + k * h             -- height function for sample points 
          s = foldl1 (+) $ map height [0..n-1] -- sum heights

-- right riemann sum, same args
rmnnrght :: (Num a, Fractional a, Enum a) => (a -> a) -> a -> a -> a -> a

rmnnrght f a b n =
    h * s
    where h = (b-a)/n                          -- width of interval
          height k = f $ a + k * h             -- height function for sample points 
          s = foldl1 (+) $ map height [1..n]   -- sum heights

