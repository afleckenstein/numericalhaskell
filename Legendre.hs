module Legendre (legendre) where
import Polynomial

legendre :: (Num t,Fractional t) => Int -> [t]
legendre 0 = [1]
legendre 1 = [0,1]
legendre n = [1/(fromIntegral n)]*([fromIntegral (2*n-1)]*[0,1]*(legendre (n-1))-[fromIntegral (n-1)]*(legendre (n-2)))
