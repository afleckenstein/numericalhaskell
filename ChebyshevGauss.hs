module ChebyshevGauss where
import Chebyshev
import Polynomial

-- returns a list of roots of a Chebyshev Polynomial of order n
chebroots :: (Num t, Floating t) => Int -> [t]
chebroots n = map cos l where numerator k = fromIntegral (2*k-1)
                              denominator = fromIntegral (2*n)
                              element k = ((numerator k)/denominator)*pi
                              l = [element k | k <- [1..n]]

-- integrates a function f via Chebyshev-Gauss quadrature,
-- using n sample points, lower bound a and upper bound b
chebgauss :: (Num t, Floating t) => (t -> t) -> Int -> t -> t -> t
chebgauss f n a b = product where weight = pi/(fromIntegral (n))
                                  newx x = ((b-a)/2) * x + ((a+b)/2)
                                  g x = (f $ newx x) * sqrt(1-x**2)
                                  heights      = map g $ chebroots n
                                  product   = ((b-a)/2)*weight * (sum heights) 
