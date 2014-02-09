-- a library of numerical differentiation functions

{- |
    finitediff is the basic difference formula, (f(x+h)-f(x))/h. "eps" is machine epsilon,
    or 2.2 * 10^-16. It's essentially as close as you can get to 0 with a double without causing problems.
-}

finitediff :: (Double -> Double) -> Double -> Double 

finitediff f c = 
    ((f cph) - (f c)) / dx
    where eps = 2.2 * (10 ** (-16))
          h = sqrt(eps)*c
          cph = c + h
          dx = cph - c
