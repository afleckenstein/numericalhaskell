module Chebyshev where
import Polynomial

-- Generate a Chebyshev polynomial of order n recursively
chebyshev :: (Num t) => Int -> [t]
chebyshev 0 = [1]
chebyshev 1 = [0,1]
chebyshev n = [0,2]*(chebyshev (n-1)) - (chebyshev (n-2))
