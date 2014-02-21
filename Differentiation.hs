-- Find the numerical derivative of a function f at a value c

finitediff :: (Double -> Double) -> Double -> Double 

finitediff f c = 
    ((f cph) - (f c)) / dx
    where eps = 2.2 * (10 ** (-16)) -- machine epsilon
          h = sqrt(eps)*c
          cph = c + h
          dx = cph - c
