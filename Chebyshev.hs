module Chebyshev where
import Polynomial

chebyshev :: (Num t) => Int -> [t]
chebyshev 0 = [1]
chebyshev 1 = [0,1]
chebyshev n = [0,2]*(chebyshev (n-1)) - (chebyshev (n-2))
