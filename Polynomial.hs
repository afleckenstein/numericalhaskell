module Polynomial 
( eval
, horner_element
, horner
, polyderiv
, polyintegrate
, newton
) where

-- Here we add lists to the num class, to implement
-- polynomials as lists of coefficients
instance Num a => Num [a] where
    -- addition
    (f:fs) + (g:gs) = f+g : fs+gs
    fs + [] = fs
    [] + gs = gs

    -- multiplication
    (f:fs) * (g:gs) = f*g : [f]*gs + fs*(g:gs)
    _ * _           = [] -- handling list ends

    --the rest of the declarations
    abs           = undefined
    signum        = map signum
    fromInteger n = [fromInteger n]
    negate        = map (\x -> -x)

-- evaluating a polynomial l at a given value x the brute force way
eval     :: (Num t, Enum t, Floating t) => [t] -> t -> t
eval l x =  sum $ zipWith (*) l [x**n | n <- [0..] ]

-- the nth horner element, given a polynomial l and a value x at which it needs
-- to be evaluated
horner_element      :: (Num t) => [t] -> t -> Int -> t
horner_element l x n
 | n == (length l)-1 = last l
 | otherwise        = (l !! n) +
                      (horner_element l x (n+1))*x

-- compute a polynomial l given as a list of coefficients at the value x
-- using horner's method.

horner     :: (Num t) => [t] -> t -> t
horner l x =  horner_element l x 0

-- calculate the derivative of a polynomial l given
-- as a list of coefficients
polyderiv   :: (Num t, Enum t) => [t] -> [t]
polyderiv l =  tail $ zipWith (*) l [0..]

-- calculate the integral of a polynomial l given
-- as a list of coefficients
polyintegrate :: (Num t, Enum t, Fractional t) => [t] -> [t]
polyintegrate l = 0 : (zipWith (/) l [1..])

-- use newton's method to find a zero given a polynomial l and initial guess x, 
-- and iterations n
newton       :: (Num t, Enum t, Fractional t, Floating t) => [t] -> t -> Int -> t
newton p x 1 =  x - (eval p x)/(eval (polyderiv p) x)
newton p x n =  prev - f/fdx where f    = eval p prev
                                   fdx  = eval (polyderiv p) prev
                                   prev = newton p x (n-1)
