module ChebyshevGauss where
import Chebyshev
import Polynomial

chebroots :: (Num t, Floating t) => Int -> [t]
chebroots n = map cos l where num k = fromIntegral (2*k-1)
                              denom = fromIntegral (2*n)
                              element k = ((num k)/denom)*pi
                              l = [element k | k <- [1..n]]

chebgauss :: (Num t, Floating t) => (t -> t) -> Int -> t
chebgauss f n = prod where weight = pi/(fromIntegral (n))
                           g x = (f x) * sqrt(1-x**2)
                           xs      = map g (chebroots n)
                           prod   = weight * (sum xs) 
